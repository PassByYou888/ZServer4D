unit IAP_IOSImp;

interface
{.$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Generics.Collections, System.Classes;

type
  EIAPException = class(Exception);

  EIAPNotSetup = class(EIAPException);

  EIAPNoLicenseKey = class(EIAPException);

  TInAppPurchase_IOS = class;

  TDownloadLengths = array of Int64;

  TProduct = class
  private
    FProductID: string;
    FPrice: Double;
    FLocalizedPrice: string;
    FLocalizedTitle: string;
    FLocalizedDescription: string;
    FDownloadable: Boolean;
    FDownloadContentLengths: TDownloadLengths;
    FDownloadContentVersion: string;
  public
    // Android supplies just a localized price string, not an actual value
    // so on Android the Price property will have this constant as a value
    const PriceNotAvailable = -1;
    constructor Create(const ProductID: string; Price: Double;
      const LocalizedPrice, LocalizedTitle, LocalizedDescription: string;
      Downloadable: Boolean; const DownloadContentLengths: TDownloadLengths;
      DownloadContentVersion: string);
    property ProductID: string read FProductID;
    property Price: Double read FPrice;
    property LocalizedPrice: string read FLocalizedPrice;
    property LocalizedTitle: string read FLocalizedTitle;
    property LocalizedDescription: string read FLocalizedDescription;
    property Downloadable: Boolean read FDownloadable;
    property DownloadContentLengths: TDownloadLengths
      read FDownloadContentLengths;
    property DownloadContentVersion: string read FDownloadContentVersion;
  end;


  TFailureKind = (ProductsRequest, Purchase);

  // This type is used by the OnProductsRequestResponse event handler below. The
  // type alias is used to help C++ work with a published generic.
  TIAPProductList = class(TList<TProduct>)
  end;

  IFMXInAppPurchaseService = interface(IInterface)
    ['{F4426C82-130F-44F3-94AF-FCC8CD197262}']
    procedure AddComponent(const Component: TInAppPurchase_IOS);
    procedure RemoveComponent(const Component: TInAppPurchase_IOS);
    function CanMakeInAppPurchases: Boolean;
    procedure QueryProducts(const ProductIDs: TStrings);
    function IsProductPurchased(const ProductID: string): Boolean;
    procedure PurchaseProduct(const ProductID: string);
    procedure ConsumeProduct(const ProductID: string);
    procedure ConsumeProducts(const ProductIDs: TStrings);
    procedure RestorePurchasedProducts;
  end;

  IInAppPurchaseCallbacks = interface
  ['{717618A6-5057-4516-B7AC-04BF5D331F6F}']
    procedure DoProductsRequestResponse(const Products: TIAPProductList; const InvalidProductIDs: TStrings);
    procedure DoError(FailureKind: TFailureKind; const ErrorMessage: string);
    procedure DoPurchaseCompleted(const ProductID: string; NewTransaction: Boolean);
    procedure DoRecordTransaction(const ProductID, TransactionID, PayReceipt: string; TransactionDate: TDateTime);
    procedure DoDownloadProgress(const ProductID, ContentID: string; TimeRemaining: Double; Progress: Single);
    procedure DoDownloadCompleted(const ProductID, ContentID, FilePath: string);
    procedure DoConsumeCompleted(const ProductID: string);
    procedure DoConsumeFailed(const ProductID, ErrorMessage: string);
  end;

  TInAppPurchase_IOS = class(TPersistent)
  private
    FProductIDs: TStrings;
    FInAppPurchaseService: IFMXInAppPurchaseService;
    FIAPBackCall:IInAppPurchaseCallbacks;
    procedure SetProductIDs(const Value: TStrings);
  public
    constructor Create(IAPBackCall:IInAppPurchaseCallbacks); virtual;
    destructor Destroy; override;

    function CanMakeInAppPurchases: Boolean;
    procedure QueryProducts;
    function IsProductPurchased(const ProductID: string): Boolean;
    procedure PurchaseProduct(const ProductID: string);
    procedure ConsumeProduct(const ProductID: string);
    procedure ConsumeProducts(const ProductIDs: TStrings);
    procedure RestorePurchasedProducts;
    property ProductIDs: TStrings read FProductIDs write SetProductIDs;
  end;

implementation

uses
  Macapi.CoreFoundation, Macapi.ObjectiveC, Macapi.ObjCRuntime, Macapi.Helpers, iOSApi.CocoaTypes, iOSApi.Foundation,
  iOSapi.UIKit, iOSapi.StoreKit, FMX.Helpers.iOS,
  System.IOUtils,
  System.TypInfo,
  FMX.Types,
  FMX.Forms,
  FMX.Consts,
  FMX.Platform;

type
  id = Pointer;

const
  FoundationFwk: string = '/System/Library/Frameworks/Foundation.framework/Foundation';
  PurchasedSuffix = '__purchased';
  QuantitySuffix = '__quantity';
  ExpiryDateSuffix = '__expiry';
  FilesSuffix = '__files';

{$REGION 'Objective C - Delphi Helpers'}

function PStrToNSStr(const AStr: string): PNSString; inline;
begin
  Result := (StrToNSStr(AStr) as ILocalObject).GetObjectID
end;

procedure SetNetTrafficIndicator(Enable: Boolean);
begin
  SharedApplication.setNetworkActivityIndicatorVisible(Enable)
end;

procedure Log(const AStr: string); overload; inline;
begin
{$IFDEF DEBUG}
  NSLog(PStrToNSStr(AStr));
{$ENDIF}
end;

procedure Log(const AStr: string; const Args: array of const); overload;
begin
{$IFDEF DEBUG}
  Log(Format(AStr, Args))
{$ENDIF}
end;

function DefaultPaymentQueue: SKPaymentQueue;
begin
  Result := TSKPaymentQueue.Wrap(TSKPaymentQueue.OCClass.defaultQueue);
end;

function StandardUserDefaults: NSUserDefaults;
begin
  Result := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.StandardUserDefaults)
end;

function SKProductToProduct(const Product: SKProduct): TProduct;
var
  ProductID: string;
  Price: Double;
  NumFormatter: NSNumberFormatter;
  LocalizedPrice: string;
  LocalizedTitle: string;
  LocalizedDescription: string;
  Downloadable: Boolean;
  DownloadContentLengths: TDownloadLengths;
  I: Integer;
  DownloadContentVersion: string;
begin
  ProductID := NSStrToStr(Product.productIdentifier);
  Price := Product.Price.doubleValue;
  NumFormatter := TNSNumberFormatter.Create;
  NumFormatter.setFormatterBehavior(NSNumberFormatterBehavior10_4);
  NumFormatter.setNumberStyle(NSNumberFormatterCurrencyStyle);
  NumFormatter.setLocale(Product.priceLocale);
  LocalizedPrice := NSStrToStr(NumFormatter.stringFromNumber(Product.Price));

  NumFormatter.release;
  LocalizedTitle := NSStrToStr(Product.LocalizedTitle);
  LocalizedDescription := NSStrToStr(Product.LocalizedDescription);
  Downloadable := Product.isDownloadable;
  if Assigned(Product.DownloadContentLengths) then
    SetLength(DownloadContentLengths, Product.DownloadContentLengths.count);
  for I := 0 to Pred(Length(DownloadContentLengths)) do
    DownloadContentLengths[I] := TNSNumber.Wrap(Product.DownloadContentLengths.objectAtIndex(I)).longLongValue;
  DownloadContentVersion := NSStrToStr(Product.DownloadContentVersion);
  Result := TProduct.Create(ProductID, Price, LocalizedPrice, LocalizedTitle, LocalizedDescription, Downloadable,
    DownloadContentLengths, DownloadContentVersion);
end;

function DownloadableContentPath: string;
var
  Paths: NSArray;
  Dir: NSString;
  FileManager: NSFileManager;
  ErrorPtr: Pointer;
  Error: NSError;
  Msg: string;
  Url: NSURL;
begin
  ErrorPtr := nil;
  Paths := TNSArray.Wrap(NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, True));
  Dir := TNSString.Wrap(Paths.objectAtIndex(0));
  Result := NSStrToStr(Dir);
  Result := TPath.Combine(Result, 'Downloads');
  Dir := StrToNSStr(Result);
  FileManager := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  if not FileManager.fileExistsAtPath(Dir) then
  begin
    FileManager.createDirectoryAtPath(Dir, True, nil, nil);
    Url := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(Dir));

    // Ensure iCloud backup doesn't take a backup of this dir as per:
    // http://developer.apple.com/library/ios/#qa/qa1719
    if not Url.setResourceValue(id(TNSNumber.OCClass.numberWithBool(True)),
      CocoaNSStringConst(FoundationFwk, 'NSURLIsExcludedFromBackupKey'), @ErrorPtr) then
    begin
      if ErrorPtr <> nil then
      begin
        Error := TNSError.Wrap(ErrorPtr);
        Msg := NSStrToStr(Error.LocalizedDescription);
      end;
    end;
  end;
end;
{$ENDREGION}

type
  TiOSInAppPurchaseService = class;
  PSKPaymentQueue = Pointer;
  PNSArray = Pointer;

  TiOSTransactionObserver = class(TOCLocal, SKPaymentTransactionObserver)
  private
    [weak] FIAPService: TiOSInAppPurchaseService;
  protected
{$REGION 'Transaction processing'}
    procedure completeTransaction(transaction: SKPaymentTransaction);
    procedure failedTransaction(transaction: SKPaymentTransaction);
    procedure restoreTransaction(transaction: SKPaymentTransaction);
    procedure recordTransaction(transaction: SKPaymentTransaction);
    procedure provideContentForProductID(ProductID: NSString; NewTransaction: Boolean);
{$ENDREGION}
{$REGION 'Download processing'}
    procedure processDownload(download: SKDownload);
{$ENDREGION}
  public
    constructor Create(const IAPService: TiOSInAppPurchaseService);
{$REGION 'SKPaymentTransactionObserver methods'}
    [MethodName('paymentQueue:updatedDownloads:')]
    procedure paymentQueueUpdatedDownloads(queue: SKPaymentQueue; updatedDownloads: NSArray); cdecl;
    [MethodName('paymentQueue:updatedTransactions:')]
    procedure paymentQueueUpdatedTransactions(queue: SKPaymentQueue; updatedTransactions: NSArray); cdecl;
    procedure paymentQueue(queue: SKPaymentQueue; removedTransactions: NSArray); overload; cdecl;
    procedure paymentQueue(queue: SKPaymentQueue; restoreCompletedTransactionsFailedWithError: NSError); overload; cdecl;
    procedure paymentQueueRestoreCompletedTransactionsFinished(queue: SKPaymentQueue); cdecl;
{$ENDREGION}
  end;

  TiOSProductsRequestDelegate = class(TOCLocal, SKProductsRequestDelegate)
  private
    [weak] FIAPService: TiOSInAppPurchaseService;
  public
    constructor Create(const IAPService: TiOSInAppPurchaseService);
    destructor Destroy; override;
{$REGION 'SKProductRequestDelegate methods'}
    procedure request(request: SKRequest; didFailWithError: NSError); cdecl;
    procedure requestDidFinish(request: SKRequest); cdecl;
    procedure productsRequest(request: SKProductsRequest;
      didReceiveResponse: SKProductsResponse); cdecl;
{$ENDREGION}
  end;

  TiOSInAppPurchaseService = class(TInterfacedObject, IFMXInAppPurchaseService)
  private
    FProductsRequest: SKProductsRequest;
    FProductsRequestDelegate: TiOSProductsRequestDelegate;
    FComponents: TList<TInAppPurchase_IOS>;
    FProducts: NSArray;
    FProductList: TIAPProductList;
    procedure DoProductsRequestResponse(const Products: TIAPProductList; const InvalidProductIDs: TStrings);
    procedure DoError(FailureKind: TFailureKind; const ErrorMessage: string);
    procedure DoPurchaseCompleted(const ProductID: string; NewTransaction: Boolean);
    procedure DoRecordTransaction(const ProductID, TransactionID, PayReceipt: string; TransactionDate: TDateTime);
    procedure DoDownloadProgress(const ProductID, ContentID: string; TimeRemaining: Double; Progress: Single);
    procedure DoDownloadCompleted(const ProductID, ContentID, FilePath: string);
  public
    constructor Create;
    destructor Destroy; override;
{$REGION 'IFMXInAppPurchaseService methods'}
    procedure AddComponent(const Component: TInAppPurchase_IOS);
    procedure RemoveComponent(const Component: TInAppPurchase_IOS);
    function CanMakeInAppPurchases: Boolean;
    procedure QueryProducts(const ProductIDs: TStrings);
    function IsProductPurchased(const ProductID: string): Boolean;
    procedure PurchaseProduct(const ProductID: string);
    procedure ConsumeProduct(const ProductID: string);
    procedure ConsumeProducts(const ProductIDs: TStrings);
    procedure RestorePurchasedProducts;
{$ENDREGION 'IFMXInAppPurchaseService methods'}
  end;

var
  InAppPurchaseService: TiOSInAppPurchaseService;
  TransactionObserver: TiOSTransactionObserver;

{$REGION 'TiOSTransactionObserver'}

constructor TiOSTransactionObserver.Create(const IAPService: TiOSInAppPurchaseService);
begin
  inherited Create;
  FIAPService := IAPService;
end;

{$REGION 'SKPaymentTransactionObserver methods'}

procedure TiOSTransactionObserver.paymentQueueUpdatedDownloads(queue: SKPaymentQueue; updatedDownloads: NSArray);
var
  Download: SKDownload;
  I: Integer;
  ProductID: string;
  ContentID: string;
  Remaining: Double;
  Progress: Single;
begin
  for I := 0 to Pred(updatedDownloads.count) do
  begin
    Download := TSKDownload.Wrap(updatedDownloads.objectAtIndex(I));
    case Download.downloadState of
      SKDownloadStateActive:
        begin
          ProductID := NSStrToStr(Download.transaction.payment.productIdentifier);
          ContentID := NSStrToStr(Download.contentIdentifier); //unique content id
          Remaining := Download.TimeRemaining; // in seconds, -1 indicates it hasn't started yet
          Progress := Download.Progress; // 0.0 - 1.0
        //Notify user of download progress
          if FIAPService <> nil then
            FIAPService.DoDownloadProgress(ProductID, ContentID, Remaining, Progress);
        end;
      SKDownloadStateFinished:
        begin
          processDownload(Download);
          if Download.transaction.transactionState = SKPaymentTransactionStateRestored then
            restoreTransaction(Download.transaction)
          else
            completeTransaction(Download.transaction);
          SetNetTrafficIndicator(False);
        end;
    end;
    Download := nil;
  end;
end;

procedure TiOSTransactionObserver.paymentQueueUpdatedTransactions(queue: SKPaymentQueue; updatedTransactions: NSArray);
var
  Transaction: SKPaymentTransaction;
  I: Integer;
begin
  for I := 0 to Pred(updatedTransactions.count) do
  begin
    Transaction := TSKPaymentTransaction.Wrap(updatedTransactions.objectAtIndex(I));
    case Transaction.transactionState of
      SKPaymentTransactionStatePurchasing:
          SetNetTrafficIndicator(True);
      SKPaymentTransactionStatePurchased:
        begin
          if (Transaction.downloads <> nil) and (Transaction.downloads.count > 0) then
            DefaultPaymentQueue.startDownloads(Transaction.downloads)
          else
          begin
            completeTransaction(Transaction);
            SetNetTrafficIndicator(False);
          end;
        end;
      SKPaymentTransactionStateRestored:
        begin
          if (Transaction.downloads <> nil) and (Transaction.downloads.count > 0) then
            DefaultPaymentQueue.startDownloads(Transaction.downloads)
          else
          begin
            restoreTransaction(Transaction);
            SetNetTrafficIndicator(False);
          end;
        end;
      SKPaymentTransactionStateFailed:
        begin
          failedTransaction(Transaction);
          SetNetTrafficIndicator(False);
        end;
    end;
  end;
end;

procedure TiOSTransactionObserver.paymentQueue(queue: SKPaymentQueue; removedTransactions: NSArray);
begin

end;

procedure TiOSTransactionObserver.paymentQueue(queue: SKPaymentQueue; restoreCompletedTransactionsFailedWithError: NSError);
begin

end;

procedure TiOSTransactionObserver.paymentQueueRestoreCompletedTransactionsFinished(queue: SKPaymentQueue);
begin

end;
{$ENDREGION 'SKPaymentTransactionObserver methods'}
{$REGION 'Transaction processing'}

procedure TiOSTransactionObserver.completeTransaction(transaction: SKPaymentTransaction);
begin
  recordTransaction(transaction);
  provideContentForProductID(transaction.payment.productIdentifier, True);
  //Final step to complete the transaction
  DefaultPaymentQueue.finishTransaction(transaction);
end;

procedure TiOSTransactionObserver.restoreTransaction(transaction: SKPaymentTransaction);
begin
  //Almost the same as completeTransaction but we have a new transaction id (& receipt)
  recordTransaction(transaction);
  provideContentForProductID(transaction.originalTransaction.payment.productIdentifier, False);
  //Final step to complete the transaction
  DefaultPaymentQueue.finishTransaction(transaction);
end;

procedure TiOSTransactionObserver.failedTransaction(transaction: SKPaymentTransaction);
begin
  if (transaction.Error.code <> SKErrorPaymentCancelled) and (FIAPService <> nil) then
    FIAPService.DoError(TFailureKind.Purchase, NSStrToStr(transaction.Error.LocalizedDescription));
  DefaultPaymentQueue.finishTransaction(transaction);
end;

procedure TiOSTransactionObserver.recordTransaction(transaction: SKPaymentTransaction);
var
  DateFormatter: NSDateFormatter;
  LString :NSString;
begin
  DateFormatter := TNSDateFormatter.Create;
  DateFormatter.setDateFormat(StrToNSStr('yyyy''-''MM''-''dd''T''HH'':''mm'':''ss''Z'));
  DateFormatter.setTimeZone(TNSTimeZone.Wrap(TNSTimeZone.OCClass.systemTimeZone));
  DateFormatter.release;
  if FIAPService <> nil then
    begin
    LString := TNSString.Wrap(TNSString.Alloc.initWithData(transaction.transactionReceipt, NSUTF8StringEncoding));

    FIAPService.DoRecordTransaction(NSStrToStr(transaction.payment.productIdentifier),
      NSStrToStr(transaction.transactionIdentifier), NSStrToStr(LString), NSDateToDateTime(transaction.TransactionDate));
    end;
end;

procedure TiOSTransactionObserver.provideContentForProductID(ProductID: NSString; NewTransaction: Boolean);
begin
  //Now trigger component event
  if FIAPService <> nil then
    FIAPService.DoPurchaseCompleted(NSStrToStr(ProductID), NewTransaction);
end;
{$ENDREGION 'Transaction processing'}
{$REGION 'Download processing'}

procedure TiOSTransactionObserver.processDownload(download: SKDownload);
var
  Path: NSString;
  FileManager: NSFileManager;
  Files: NSArray;
  DownloadedFilePaths: NSMutableArray;
  Dir: NSString;
  DirP: string;
  I: Integer;
  LFile: NSString;
  FullPathSrc: NSString;
  FullPathDest: NSString;
begin
  // Get the path string from the URL
  Path := download.contentURL.Path;
  // Source directory is Contents subdirectory
  Path := Path.stringByAppendingPathComponent(StrToNSStr('Contents'));
  FileManager := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  Files := FileManager.contentsOfDirectoryAtPath(Path, nil);
  if Files = nil then
  begin
    // Download didn't turn up so let's bail - we may be here from an
    // app delete/reinstall/restart, and require a Restore operation to
    // pull the file again
    Exit
  end;
  // Locate destination directory
  DirP := DownloadableContentPath;
  Dir := StrToNSStr(DirP);
  DownloadedFilePaths := TNSMutableArray.Create;
  for I := 0 to Pred(Files.count) do
  begin
    LFile := TNSString.Wrap(Files.objectAtIndex(I));
    FullPathSrc := Path.stringByAppendingPathComponent(LFile);
    FullPathDest := Dir.stringByAppendingPathComponent(LFile);
    // Make a note of the full path to the file
    DownloadedFilePaths.addObject((FullPathDest as ILocalObject).GetObjectID);
    // Avoid error caused if you overwrite a file
    FileManager.removeItemAtPath(FullPathDest, nil);
    FileManager.moveItemAtPath(FullPathSrc, FullPathDest, nil);
    //Now trigger component event
    if FIAPService <> nil then
      FIAPService.DoDownloadCompleted(NSStrToStr(download.transaction.payment.productIdentifier),
        NSStrToStr(download.contentIdentifier), NSStrToStr(FullPathDest))
  end;
end;
{$ENDREGION 'Download processing'}
{$ENDREGION 'TiOSTransactionObserver'}
{$REGION 'TiOSProductRequestDelegate'}

constructor TiOSProductsRequestDelegate.Create(const IAPService: TiOSInAppPurchaseService);
begin
  inherited Create;
  FIAPService := IAPService;
end;

destructor TiOSProductsRequestDelegate.Destroy;
begin
  inherited;
end;

{$REGION 'SKProductRequestDelegate methods'}

procedure TiOSProductsRequestDelegate.productsRequest(request: SKProductsRequest; didReceiveResponse: SKProductsResponse);
var
  Product: SKProduct;
  LocalProduct: TProduct;
  Products: NSArray;
  InvalidProducts: NSArray;
  ProductID: NSString;
  I: Integer;
  InvalidProductIDList: TStrings;
begin
  FIAPService.FProductList.Clear;
  InvalidProductIDList := TStringList.Create;
  if FIAPService.FProducts <> nil then
    FIAPService.FProducts.release;
  Products := didReceiveResponse.Products;
  FIAPService.FProducts := Products;
  FIAPService.FProducts.retain;
  InvalidProducts := didReceiveResponse.invalidProductIdentifiers;
  if (Products <> nil) and (Products.count > 0) then
    for I := 0 to Pred(Products.count) do
    begin
      Product := TSKProduct.Wrap(Products.objectAtIndex(I));
      LocalProduct := SKProductToProduct(Product);
      FIAPService.FProductList.Add(LocalProduct);
    end;
  if (InvalidProducts <> nil) and (InvalidProducts.count > 0) then
    for I := 0 to Pred(InvalidProducts.count) do
    begin
      ProductID := TNSString.Wrap(InvalidProducts.objectAtIndex(I));
      InvalidProductIDList.Add(NSStrToStr(ProductID));
    end;
  if FIAPService <> nil then
    FIAPService.DoProductsRequestResponse(FIAPService.FProductList, InvalidProductIDList)
end;

procedure TiOSProductsRequestDelegate.request(request: SKRequest; didFailWithError: NSError);
var
  Msg: string;
begin
  Msg := NSStrToStr(didFailWithError.LocalizedDescription);
  if Msg = '' then
    Msg := SITunesConnectionError;
  if FIAPService <> nil then
    FIAPService.DoError(TFailureKind.ProductsRequest, Msg);
  FIAPService.FProductsRequest := nil;
  SetNetTrafficIndicator(False);

end;

procedure TiOSProductsRequestDelegate.requestDidFinish(request: SKRequest);
begin
  FIAPService.FProductsRequest := nil;
  SetNetTrafficIndicator(False);

end;
{$ENDREGION 'SKProductRequestDelegate methods'}
{$ENDREGION 'TiOSProductRequestDelegate'}
{$REGION 'TiOSInAppPurchaseService'}

constructor TiOSInAppPurchaseService.Create;
begin
  inherited;
  FComponents := TList<TInAppPurchase_IOS>.Create;
  TransactionObserver := TiOSTransactionObserver.Create(Self);
  DefaultPaymentQueue.addTransactionObserver(TransactionObserver.GetObjectID);
  FProductsRequestDelegate := TiOSProductsRequestDelegate.Create(Self);
  FProductList := TIAPProductList.Create;
end;

destructor TiOSInAppPurchaseService.Destroy;
begin
  DefaultPaymentQueue.removeTransactionObserver(TransactionObserver);
  TransactionObserver := nil;
  FProductsRequestDelegate := nil;
  if FProducts <> nil then
    FProducts.release;
  FProductList.Clear;
  while FComponents.count > 0 do
    FComponents.Delete(0);
  FComponents := nil;
  inherited;
end;

procedure TiOSInAppPurchaseService.DoProductsRequestResponse(const Products: TIAPProductList;
  const InvalidProductIDs: TStrings);
var
  Component: TInAppPurchase_IOS;
begin
  for Component in FComponents do
      Component.FIAPBackCall.DoProductsRequestResponse(Products, InvalidProductIDs);
end;

procedure TiOSInAppPurchaseService.DoError(FailureKind: TFailureKind; const ErrorMessage: string);
var
  Component: TInAppPurchase_IOS;
begin
  for Component in FComponents do
      Component.FIAPBackCall.DoError(FailureKind, ErrorMessage);
end;

procedure TiOSInAppPurchaseService.DoPurchaseCompleted(const ProductID: string; NewTransaction: Boolean);
var
  Component: TInAppPurchase_IOS;
  Defaults: NSUserDefaults;
begin
  // Update user defaults to record purchase against this product ID
  Defaults := StandardUserDefaults;
  Defaults.setBool(True, StrToNSStr(ProductID + '_purchased'));

  for Component in FComponents do
      Component.FIAPBackCall.DoPurchaseCompleted(ProductID, NewTransaction);
end;

procedure TiOSInAppPurchaseService.DoRecordTransaction(const ProductID, TransactionID, PayReceipt: string; TransactionDate: TDateTime);
var
  Component: TInAppPurchase_IOS;
begin
  for Component in FComponents do
      Component.FIAPBackCall.DoRecordTransaction(ProductID, TransactionID, PayReceipt, TransactionDate);
end;

procedure TiOSInAppPurchaseService.DoDownloadProgress(const ProductID, ContentID: string;
  TimeRemaining: Double; Progress: Single);
var
  Component: TInAppPurchase_IOS;
begin
  for Component in FComponents do
      Component.FIAPBackCall.DoDownloadProgress(ProductID, ContentID, TimeRemaining, Progress);
end;

procedure TiOSInAppPurchaseService.DoDownloadCompleted(const ProductID, ContentID, FilePath: string);
var
  Component: TInAppPurchase_IOS;
begin
  for Component in FComponents do
      Component.FIAPBackCall.DoDownloadCompleted(ProductID, ContentID, FilePath);
end;

{$REGION 'IFMXInAppPurchaseService methods'}

procedure TiOSInAppPurchaseService.AddComponent(const Component: TInAppPurchase_IOS);
begin
  if not FComponents.Contains(Component) then
    FComponents.Add(Component);
end;

procedure TiOSInAppPurchaseService.RemoveComponent(const Component: TInAppPurchase_IOS);
begin
  if Component <> nil then
    FComponents.Remove(Component);
end;

function TiOSInAppPurchaseService.CanMakeInAppPurchases: Boolean;
begin
  Result := TSKPaymentQueue.OCClass.canMakePayments;
end;


procedure TiOSInAppPurchaseService.QueryProducts(const ProductIDs: TStrings);
var
  ProductIDsArray: NSMutableArray;
  ProductIDsSet: NSSet;
  ProductID: string;
begin
  //Check FProductsRequest isn't already in use
  if FProductsRequest <> nil then
    raise EIAPException.Create(SProductsRequestInProgress);
  ProductIDsArray := TNSMutableArray.Create;
  for ProductID in ProductIDs do
    ProductIDsArray.addObject(PStrToNSStr(ProductID));
  ProductIDsSet := TNSSet.Wrap(TNSSet.OCClass.setWithArray(ProductIDsArray));
  FProductsRequest := TSKProductsRequest.Wrap(TSKProductsRequest.Alloc.initWithProductIdentifiers(ProductIDsSet));
  ProductIDsArray.release;
  ProductIDsSet.release;
  FProductsRequest.setDelegate((FProductsRequestDelegate as ILocalObject).GetObjectID);
  //Set off network activity indicator
  SetNetTrafficIndicator(True);
  //Now initiate the products request
  FProductsRequest.start;
end;

procedure TiOSInAppPurchaseService.RestorePurchasedProducts;
begin
  DefaultPaymentQueue.restoreCompletedTransactions;
end;

function TiOSInAppPurchaseService.IsProductPurchased(const ProductID: string): Boolean;
var
  Defaults: NSUserDefaults;
begin
  // Check in the user defaults and see if we've previously
  // recorded a purchase against this product ID
  Defaults := StandardUserDefaults;
  Result := Defaults.boolForKey(StrToNSStr(ProductID + '_purchased'));
end;

procedure TiOSInAppPurchaseService.PurchaseProduct(const ProductID: string);
var
  I: Integer;
  LProduct: TProduct;
  Product: SKProduct;
  Payment: SKPayment;
begin
  if IsProductPurchased(ProductID) then
    raise EIAPException.Create(SIAPAlreadyPurchased);
  if FProducts <> nil then
    for I := 0 to Pred(FProductList.count) do
    begin
      LProduct := FProductList[I];
      if LProduct.ProductID = ProductID then
      begin
        Product := TSKProduct.Wrap(FProducts.objectAtIndex(I));
        Payment := TSKPayment.Wrap(TSKPayment.OCClass.paymentWithProduct(Product));
        DefaultPaymentQueue.addPayment(Payment);
        Break;
      end;
    end;
end;

procedure TiOSInAppPurchaseService.ConsumeProduct(const ProductID: string);
var
  I: Integer;
  LProduct: TProduct;
  Product: SKProduct;
  Payment: SKPayment;
begin
  if FProducts <> nil then
    for I := 0 to Pred(FProductList.count) do
    begin
      LProduct := FProductList[I];
      if LProduct.ProductID = ProductID then
      begin
        Product := TSKProduct.Wrap(FProducts.objectAtIndex(I));
        Payment := TSKPayment.Wrap(TSKPayment.OCClass.paymentWithProduct(Product));
        DefaultPaymentQueue.addPayment(Payment);
        Break;
      end;
    end;
end;

procedure TiOSInAppPurchaseService.ConsumeProducts(const ProductIDs: TStrings);
var
  ProductID: string;
begin
  for ProductID in ProductIDs do
    ConsumeProduct(ProductID);
end;

{$ENDREGION 'IFMXInAppPurchaseService methods'}
{$ENDREGION 'TiOSInAppBillingService'}

{ TCustomInAppBilling }

constructor TInAppPurchase_IOS.Create(IAPBackCall:IInAppPurchaseCallbacks);
var
  IAPIntf: IInterface;
begin
  inherited Create;
  FProductIDs := TStringList.Create;
  FInAppPurchaseService := InAppPurchaseService;
  FInAppPurchaseService.AddComponent(Self);
  FIAPBackCall:=IAPBackCall;
end;

destructor TInAppPurchase_IOS.Destroy;
begin
  if FInAppPurchaseService <> nil then
    FInAppPurchaseService.RemoveComponent(Self);
  FInAppPurchaseService := nil;
  inherited Destroy;
end;

function TInAppPurchase_IOS.CanMakeInAppPurchases: Boolean;
begin
  Result := False;
  if FInAppPurchaseService <> nil then
    Result := FInAppPurchaseService.CanMakeInAppPurchases
end;

function TInAppPurchase_IOS.IsProductPurchased(const ProductID: string): Boolean;
begin
  Result := False;
  if FInAppPurchaseService <> nil then
    Result := FInAppPurchaseService.IsProductPurchased(ProductID)
end;

procedure TInAppPurchase_IOS.PurchaseProduct(const ProductID: string);
begin
  if FInAppPurchaseService <> nil then
    FInAppPurchaseService.PurchaseProduct(ProductID)
end;

procedure TInAppPurchase_IOS.ConsumeProduct(const ProductID: string);
begin
  if FInAppPurchaseService <> nil then
    FInAppPurchaseService.ConsumeProduct(ProductID)
end;

procedure TInAppPurchase_IOS.ConsumeProducts(const ProductIDs: TStrings);
begin
  if FInAppPurchaseService <> nil then
    FInAppPurchaseService.ConsumeProducts(ProductIDs)
end;

procedure TInAppPurchase_IOS.QueryProducts;
begin
  if FInAppPurchaseService <> nil then
    FInAppPurchaseService.QueryProducts(ProductIDs)
end;

procedure TInAppPurchase_IOS.RestorePurchasedProducts;
begin
  if FInAppPurchaseService <> nil then
    FInAppPurchaseService.RestorePurchasedProducts
end;

procedure TInAppPurchase_IOS.SetProductIDs(const Value: TStrings);
begin
  FProductIDs.Assign(Value);
end;

{ TProduct }

constructor TProduct.Create(const ProductID: string; Price: Double;
  const LocalizedPrice, LocalizedTitle, LocalizedDescription: string; Downloadable: Boolean;
  const DownloadContentLengths: TDownloadLengths; DownloadContentVersion: string);
begin
  inherited Create;
  FProductID := ProductID;
  FPrice := Price;
  FLocalizedPrice := LocalizedPrice;
  FLocalizedTitle := LocalizedTitle;
  FLocalizedDescription := LocalizedDescription;
  FDownloadable := Downloadable;
  FDownloadContentLengths := DownloadContentLengths;
  FDownloadContentVersion := DownloadContentVersion;
end;

var
  StoreKitFramework: HMODULE;

initialization
  StoreKitFramework := LoadLibrary(libStoreKit);
  if StoreKitFramework > 0 then
    InAppPurchaseService := TiOSInAppPurchaseService.Create;

  AddEnumElementAliases(TypeInfo(TFailureKind), ['fkProductsRequest', 'fkPurchase']);
//  RegisterFmxClasses([TInAppPurchase_IOS]);

finalization
  if StoreKitFramework > 0 then
  begin
    InAppPurchaseService := nil;
    FreeLibrary(StoreKitFramework);
  end;

  RemoveEnumElementAliases(TypeInfo(TFailureKind));

end.
 
