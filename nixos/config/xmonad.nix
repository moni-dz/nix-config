''
  -- fortuneteller2k's XMonad config
  -- This file is managed by NixOS, don't edit it directly!

  import XMonad

  import XMonad.Actions.CycleWS

  import XMonad.Hooks.DynamicLog
  import XMonad.Hooks.EwmhDesktops
  import XMonad.Hooks.FadeInactive
  import XMonad.Hooks.InsertPosition
  import XMonad.Hooks.ManageDocks
  import XMonad.Hooks.ManageHelpers
  import XMonad.Hooks.Place
  import XMonad.Hooks.SetWMName

  import XMonad.Layout.Grid
  import XMonad.Layout.NoBorders
  import XMonad.Layout.ShowWName
  import XMonad.Layout.Spacing
  import XMonad.Layout.Tabbed
  import XMonad.Layout.ThreeColumns
  import XMonad.Layout.ToggleLayouts

  import XMonad.Prompt
  import XMonad.Prompt.FuzzyMatch
  import XMonad.Prompt.Shell

  import XMonad.Util.EZConfig
  import XMonad.Util.Run
  import XMonad.Util.SpawnOnce

  import Data.Char
  import Data.Monoid

  import System.IO
  import System.Exit

  import qualified DBus                     as D
  import qualified DBus.Client              as D
  import qualified Codec.Binary.UTF8.String as UTF8
  import qualified XMonad.StackSet          as W
  import qualified Data.Map                 as M

  -- 10 workspaces should be enough
  ws = ["A","B","C","D","E","F","G","H","I","J"]

  fontFamily = "xft:FantasqueSansMono Nerd Font:size=10:antialias=true:hinting=true"
  fontFamilyLarge = "xft:FantasqueSansMono Nerd Font:size=16:style=Bold:antialias=true:hinting=true"

  keybindings =
    [ ("M-<Return>",                 spawn "alacritty")
    , ("M-d",                        shellPrompt promptConfig)
    , ("M-q",                        kill)
    , ("M-w",                        spawn "emacs")
    , ("M-<F2>",                     spawn "qutebrowser")
    , ("M-e",                        sendMessage ToggleLayout)
    , ("M-<Tab>",                    sendMessage NextLayout)
    , ("M-n",                        refresh)
    , ("M-s",                        windows W.swapMaster)
    , ("M--",                        sendMessage Shrink)
    , ("M-=",                        sendMessage Expand)
    , ("M-t",                        withFocused toggleFloat)
    , ("M-,",                        sendMessage (IncMasterN 1))
    , ("M-.",                        sendMessage (IncMasterN (-1)))
    , ("C-<Left>",                   prevWS)
    , ("C-<Right>",                  nextWS)
    , ("<Print>",                    spawn "/home/fortuneteller2k/.config/scripts/screenshot.sh wind")
    , ("M-<Print>",                  spawn "/home/fortuneteller2k/.config/scripts/screenshot.sh area")
    , ("M-S-s",                      spawn "/home/fortuneteller2k/.config/scripts/screenshot.sh full")
    , ("M-S-q",                      io (exitWith ExitSuccess))
    , ("M-S-<Delete>",               spawn "slock")
    , ("M-S-c",                      withFocused $ \w -> spawn ("xkill -id " ++ show w))
    , ("M-S-r",                      spawn $ "xmonad --restart")
    , ("M-S-<Left>",                 shiftToPrev >> prevWS)
    , ("M-S-<Right>",                shiftToNext >> nextWS)
    , ("M-<Left>",                   windows W.focusUp)
    , ("M-<Right>",                  windows W.focusDown)
    , ("M-S-<Tab>",                  sendMessage FirstLayout)
    , ("<XF86AudioMute>",            spawn "/home/fortuneteller2k/.config/scripts/volume.sh mute")
    , ("<XF86AudioRaiseVolume>",     spawn "/home/fortuneteller2k/.config/scripts/volume.sh up")
    , ("<XF86AudioLowerVolume>",     spawn "/home/fortuneteller2k/.config/scripts/volume.sh down")
    , ("<XF86AudioPlay>",            spawn "mpc toggle")
    , ("<XF86AudioPrev>",            spawn "mpc prev")
    , ("<XF86AudioNext>",            spawn "mpc next")
    , ("<XF86MonBrightnessUp>",      spawn "brightnessctl s +10%")
    , ("<XF86MonBrightnessDown>",    spawn "brightnessctl s 10%-")
    ]
    ++
    [ (otherModMasks ++ "M-" ++ key, action tag)
        | (tag, key) <- zip ws (map (\x -> show x) ([1..9] ++ [0]))
        , (otherModMasks, action) <- [ ("", windows . W.greedyView)
                                     , ("S-", windows . W.shift)]
    ]
    where
      toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                then W.sink w s
                                else (W.float w (W.RationalRect 0.15 0.15 0.7 0.7) s))

  promptConfig = def
    { font                = fontFamily
    , bgColor             = "#16161c"
    , fgColor             = "#fdf0ed"
    , bgHLight            = "#e95678"
    , fgHLight            = "#16161c"
    , borderColor         = "#e95678"
    , promptBorderWidth   = 0
    , position            = Top
    , height              = 20
    , historySize         = 256
    , historyFilter       = id
    , showCompletionOnTab = False
    , searchPredicate     = fuzzyMatch
    , sorter              = fuzzySort
    , defaultPrompter     = id $ map toLower
    , alwaysHighlight     = True
    , maxComplRows        = Just 5
    }

  layouts = avoidStruts $ tiled ||| Mirror tiled ||| tabs ||| centeredMaster ||| grid
    where
       tiled = gaps 4 4 $ toggleLayouts maximized (smartBorders (Tall 1 (3/100) (1/2)))
       centeredMaster = gaps 4 4 $ toggleLayouts maximized (smartBorders (ThreeColMid 1 (3/100) (1/2)))
       tabs = gaps 8 0 $ noBorders (tabbed shrinkText tabTheme)
       grid = gaps 4 4 $ toggleLayouts maximized (smartBorders Grid)
       maximized = smartBorders Full
       gaps n k = spacingRaw False (Border n n n n) True (Border k k k k) True

  tabTheme = def
    { fontName            = fontFamily
    , activeColor         = "#e95678"
    , inactiveColor       = "#16161c"
    , urgentColor         = "#ee64ae"
    , activeTextColor     = "#16161c"
    , inactiveTextColor   = "#fdf0ed"
    , urgentTextColor     = "#16161c"
    , activeBorderWidth   = 0
    , inactiveBorderWidth = 0
    , urgentBorderWidth   = 0
    }

  wnameTheme = def
    { swn_font    = fontFamilyLarge
    , swn_bgcolor = "#e95678"
    , swn_color   = "#16161c"
    , swn_fade    = 2
    }

  windowRules = placeHook (smart (0.5, 0.5))
    <+> composeAll
    [ className  =? "Gimp"                                   --> doFloat
    , (className =? "Ripcord" <&&> title =? "Preferences")   --> doFloat
    , className  =? "Xmessage"                               --> doFloat
    , className  =? "Peek"                                   --> doFloat
    , className  =? "Xephyr"                                 --> doFloat
    , className  =? "Sxiv"                                   --> doFloat
    , className  =? "mpv"                                    --> doFloat
    , resource   =? "desktop_window"                         --> doIgnore
    , resource   =? "kdesktop"                               --> doIgnore
    , isDialog                                               --> doF W.swapUp <+> doFloat ]
    <+> insertPosition End Newer -- same effect as attachaside patch in dwm
    <+> manageDocks
    <+> manageHook defaultConfig

  autostart = do
    spawnOnce "xsetroot -cursor_name left_ptr &"
    spawnOnce "systemctl --user restart polybar &"
    spawnOnce "xwallpaper --zoom /etc/nixos/nixos/config/wallpapers/horizon.jpg &"
    spawnOnce "xidlehook --not-when-fullscreen --not-when-audio --timer 600 slock \'\' &"
    setWMName "LG3D"

  dbusClient = do
      dbus <- D.connectSession
      D.requestName dbus (D.busName_ "org.xmonad.log") opts
      return dbus
    where
      opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  dbusOutput dbus str =
    let
      opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ UTF8.decodeString str]
    in
      D.emit dbus $ signal { D.signalBody = body }

  polybarHook dbus = dynamicLogWithPP $ xmobarPP
    { ppOutput = dbusOutput dbus
    , ppOrder  = \(_:l:_:_) -> [l]
    }

  main' dbus = xmonad . docks . ewmh $ def
    { focusFollowsMouse  = True
    , clickJustFocuses   = True
    , borderWidth        = 2
    , modMask            = mod1Mask
    , workspaces         = ws
    , normalBorderColor  = "#2e303e"
    , focusedBorderColor = "#e95678"
    , layoutHook         = showWName' wnameTheme layouts
    , manageHook         = windowRules
    , logHook            = fadeInactiveLogHook 0.95 <+> polybarHook dbus
    , handleEventHook    = fullscreenEventHook <+> ewmhDesktopsEventHook
    , startupHook        = autostart
    } `additionalKeysP` keybindings

  main = dbusClient >>= main' -- "that was easy, xmonad rocks!"
''
