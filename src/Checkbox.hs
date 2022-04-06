module Checkbox where


import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Events     as UI
import qualified Graphics.UI.Threepenny.Elements   as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

data CheckboxEntry = CheckboxEntry
    { _elementTE :: Element
    , _userTE    :: Tidings Bool
    }

instance Widget CheckboxEntry where getElement = _elementTE

userCheck :: CheckboxEntry -> Tidings Bool
userCheck = _userTE

entry :: Behavior Bool -> UI CheckboxEntry
entry bValue = do 
    input <- UI.input # set UI.type_ "checkbox"

    window <- askWindow
    liftIOLater $ onChange bValue $ \s -> runUI window $ void $ do
        element input # set UI.checked s

    let _elementTE = input
        _userTE    = tidings bValue $ UI.checkedChange input
    return CheckboxEntry {..}
