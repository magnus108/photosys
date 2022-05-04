module Config where

data Config = Config
    { dataTabSelectionFile :: FilePath
    ,datastoreLoan     :: FilePath
    ,datastoreUser      :: FilePath
    ,datastoreToken      :: FilePath
    ,datastoreTab         :: FilePath
    ,datastoreItem         :: FilePath
    ,datastoreHistory       :: FilePath
    ,datastoreHistoryHandIn  :: FilePath
    ,datastoreCount          :: FilePath
    ,datastoreTime          :: FilePath
    ,datastoreRepair          :: FilePath
    ,exportFile              :: FilePath
    }


