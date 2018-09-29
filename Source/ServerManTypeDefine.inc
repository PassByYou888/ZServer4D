// define global service type
type
  TServerType = (stUnknow,
    stManager,      // Infinite deploy
    stDatabase,     // only limitations deploy
    stDataStore,    // only limitations deploy
    stFileStore,    // Infinite deploy
    stLog,          // Infinite deploy
    stLogic,        // Infinite deploy
    stWorld,        // Infinite deploy, Game style
    stHall,         // Infinite deploy, Game style
    stHall2Home,    // Infinite deploy, Game style
    stHome,         // Infinite deploy, Game style
    stPayment,      // Infinite deploy
    stPaymentQuery, // Infinite deploy
    stFOGCompute,   // Infinite deploy
    stCloudCompute, // Infinite deploy
    stHPCCompute,   // Infinite deploy
    stMatrix,       // Infinite deploy
    stVec,          // Infinite deploy
    stLearn,        // Infinite deploy
    stImage,        // Infinite deploy
    stWave,         // Infinite deploy
    stOCR,          // Infinite deploy
    stEmulator      // Infinite deploy
    );
  TServerTypes = set of TServerType;

const
  climitationsServerType: TServerTypes = [stDatabase, stDataStore];
