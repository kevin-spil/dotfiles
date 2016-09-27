import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Actions.NoBorders
import XMonad.Config.Xfce
import Data.Monoid
import XMonad.Actions.CopyWindow
import XMonad.Layout.LayoutModifier

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "Terminator"    --> doShift "1"
    , className =? "Screenkey"     --> doIgnore
    , className =? "Xfce4-notifyd" --> doIgnore
    ]

main :: IO ()
main = xmonad $ xfceLayout

xfceLayout :: XConfig (ToggleLayouts (ModifiedLayout WithBorder Full)
                      (ModifiedLayout AvoidStruts
                      (ModifiedLayout AvoidStruts
                      (Choose Tall (Choose (Mirror Tall) Full)))))

xfceLayout = xfceConfig
             { modMask = mod4Mask
             , workspaces = map show ([1..9] :: [Int])
             , manageHook = myManageHook <+> manageHook xfceConfig
             , borderWidth        = 2
             , terminal           = "terminator"
             , normalBorderColor  = "#000000"
             , focusFollowsMouse  = False
             , layoutHook = toggleLayouts (noBorders Full) $
                            avoidStruts $ layoutHook xfceConfig
             } `additionalKeys` addKeys


addKeys :: [((KeyMask, KeySym), X ())]
addKeys =
    [ ((mod4, xK_bracketright), spawn "amixer --quiet set Master 5%+ ; amixer get Master | egrep -o \"[0-9]+%\" | head -1 | dzen2 -p 1")
    , ((mod4, xK_bracketleft), spawn "amixer --quiet set Master 5%- ; amixer get Master | egrep -o \"[0-9]+%\" | head -1 | dzen2 -p 1")
    , ((mod4, xK_quoteright), spawn "amixer --quiet set Master toggle")
    , ((0, xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 10")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((mod4, xK_F9), spawn disableTouch)
    , ((mod4 .|. shiftMask, xK_f  ), spawn "thunar")
    , ((0, xK_Print                ), spawn "scrot")
    , ((mod4, xK_Print             ), spawn "xfce4-screenshooter -r")
    , ((mod4, xK_n                 ), viewEmptyWorkspace)
    , ((mod4 .|. shiftMask, xK_n   ), tagToEmptyWorkspace)
    , ((mod4, xK_b                 ), spawn "toggleXmobar")
    , ((mod4, xK_q                 ), viewScreen 0)
    , ((mod4, xK_w                 ), viewScreen 1)
    , ((mod4, xK_e                 ), viewScreen 2)
    , ((mod1Mask, xK_Tab           ), windows W.focusDown)
    , ((mod4 .|. shiftMask, xK_x   ), sendMessage ToggleLayout)
    , ((mod4 .|. shiftMask, xK_z   ), spawn "slock")
    , ((mod4, xK_x                 ), withFocused toggleBorder)
    , ((mod4, xK_p                 ), spawn "dmenu_run -i")
    , ((mod4, xK_d                 ), spawn "xfce4-dict")
    , ((mod4, xK_r                 ), broadcastMessage ReleaseResources >> restart "xmonad" True)
    ] where mod4         = mod4Mask
            disableTouch = "synclient TouchpadOff=$(synclient -l" ++
                           "| grep -c 'TouchpadOff.*=.*0')"

{- Available keys
| <Backspace>    | <Tab>         | <Return>      | <Pause>         |
| <Scroll_lock>  | <Sys_Req>     | <Print>       | <Escape>, <Esc> |
| <Delete>       | <Home>        | <Left>, <L>   | <Up>, <U>       |
| <Right>, <R>   | <Down>, <D>   | <Page_Up>     | <Page_Down>     |
| <End>          | <Insert>      | <Break>       | <Space>         |
| <F1>-<F24>     | <KP_Space>    | <KP_Tab>      | <KP_Enter>      |
| <KP_F1>        | <KP_F2>       | <KP_F3>       | <KP_F4>         |
| <KP_Home>      | <KP_Left>     | <KP_Up>       | <KP_Right>      |
| <KP_Down>      | <KP_Prior>    | <KP_Page_Up>  | <KP_Next>       |
| <KP_Page_Down> | <KP_End>      | <KP_Begin>    | <KP_Insert>     |
| <KP_Delete>    | <KP_Equal>    | <KP_Multiply> | <KP_Add>        |
| <KP_Separator> | <KP_Subtract> | <KP_Decimal>  | <KP_Divide>     |
| <KP_0>-<KP_9>  |               |               |                 |

| <XF86ModeLock>         | <XF86MonBrightnessUp>  | <XF86MonBrightnessDown> |
| <XF86KbdLightOnOff>    | <XF86KbdBrightnessUp>  | <XF86KbdBrightnessDown> |
| <XF86Standby>          | <XF86AudioLowerVolume> | <XF86AudioMute>         |
| <XF86AudioRaiseVolume> | <XF86AudioPlay>        | <XF86AudioStop>         |
| <XF86AudioPrev>        | <XF86AudioNext>        | <XF86HomePage>          |
| <XF86Mail>             | <XF86Start>            | <XF86Search>            |
| <XF86AudioRecord>      | <XF86Calculator>       | <XF86Memo>              |
| <XF86ToDoList>         | <XF86Calendar>         | <XF86PowerDown>         |
| <XF86ContrastAdjust>   | <XF86RockerUp>         | <XF86RockerDown>        |
| <XF86RockerEnter>      | <XF86Back>             | <XF86Forward>           |
| <XF86Stop>             | <XF86Refresh>          | <XF86PowerOff>          |
| <XF86WakeUp>           | <XF86Eject>            | <XF86ScreenSaver>       |
| <XF86WWW>              | <XF86Sleep>            | <XF86Favorites>         |
| <XF86AudioPause>       | <XF86AudioMedia>       | <XF86MyComputer>        |
| <XF86VendorHome>       | <XF86LightBulb>        | <XF86Shop>              |
| <XF86History>          | <XF86OpenURL>          | <XF86AddFavorite>       |
| <XF86HotLinks>         | <XF86BrightnessAdjust> | <XF86Finance>           |
| <XF86Community>        | <XF86AudioRewind>      | <XF86XF86BackForward>   |
| <XF86Launch0-9>        | <XF86LaunchA-F>        | <XF86ApplicationLeft>   |
| <XF86ApplicationRight> | <XF86Book>             | <XF86CD>                |
| <XF86Calculater>       | <XF86Clear>            | <XF86Close>             |
| <XF86Copy>             | <XF86Cut>              | <XF86Display>           |
| <XF86DOS>              | <XF86Documents>        | <XF86Excel>             |
| <XF86Explorer>         | <XF86Game>             | <XF86Go>                |
| <XF86iTouch>           | <XF86LogOff>           | <XF86Market>            |
| <XF86Meeting>          | <XF86MenuKB>           | <XF86MenuPB>            |
| <XF86MySites>          | <XF86New>              | <XF86News>              |
| <XF86OfficeHome>       | <XF86Open>             | <XF86Option>            |
| <XF86Paste>            | <XF86Phone>            | <XF86Q>                 |
| <XF86Reply>            | <XF86Reload>           | <XF86RotateWindows>     |
| <XF86RotationPB>       | <XF86RotationKB>       | <XF86Save>              |
| <XF86ScrollUp>         | <XF86ScrollDown>       | <XF86ScrollClick>       |
| <XF86Send>             | <XF86Spell>            | <XF86SplitScreen>       |
| <XF86Support>          | <XF86TaskPane>         | <XF86Terminal>          |
| <XF86Tools>            | <XF86Travel>           | <XF86UserPB>            |
| <XF86User1KB>          | <XF86User2KB>          | <XF86Video>             |
| <XF86WheelButton>      | <XF86Word>             | <XF86Xfer>              |
| <XF86ZoomIn>           | <XF86ZoomOut>          | <XF86Away>              |
| <XF86Messenger>        | <XF86WebCam>           | <XF86MailForward>       |
| <XF86Pictures>         | <XF86Music>            | <XF86TouchpadToggle>    |
| <XF86_Switch_VT_1-12>  | <XF86_Ungrab>          | <XF86_ClearGrab>        |
| <XF86_Next_VMode>      | <XF86_Prev_VMode>      |                         |
-}
