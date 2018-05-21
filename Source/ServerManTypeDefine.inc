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
    stHPCMatcher,   // Infinite deploy
    stHPCSort,      // Infinite deploy
    stTranslate,    // Infinite deploy
    stMatrix,       // Infinite deploy
    stLearn         // Infinite deploy
    );
  TServerTypes = set of TServerType;

const
  climitationsServerType: TServerTypes = [stDatabase, stDataStore];
