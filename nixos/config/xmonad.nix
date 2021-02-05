''
  -- fortuneteller2k's XMonad config
  -- This file is managed by NixOS, don't edit it directly!

  import XMonad

  import XMonad.Actions.CycleWS
  import XMonad.Actions.Sift
  import XMonad.Actions.TiledWindowDragging

  import XMonad.Hooks.DynamicLog
  import XMonad.Hooks.EwmhDesktops
  import XMonad.Hooks.InsertPosition
  import XMonad.Hooks.ManageDocks
  import XMonad.Hooks.ManageHelpers
  import XMonad.Hooks.Place
  import XMonad.Hooks.SetWMName
  import XMonad.Hooks.WindowSwallowing

  import XMonad.Layout.DraggingVisualizer
  import XMonad.Layout.Grid
  import XMonad.Layout.NoBorders
  import XMonad.Layout.Renamed
  import XMonad.Layout.Spacing
  import XMonad.Layout.Tabbed
  import XMonad.Layout.ThreeColumns
  import XMonad.Layout.ToggleLayouts

  import XMonad.Prompt
  import XMonad.Prompt.FuzzyMatch
  import XMonad.Prompt.Shell

  import XMonad.Util.EZConfig
  import XMonad.Util.NamedScratchpad
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

  -- mod key
  modkey = mod1Mask

  -- 10 workspaces should be enough
  ws = ["A","B","C","D","E","F","G","H","I","J"]

  -- default font
  fontFamily = "xft:FantasqueSansMono Nerd Font:size=10:antialias=true:hinting=true"

  keybindings =
    [ ("M-<Return>",                 spawn term)
    , ("M-`",                        namedScratchpadAction scratchpads "terminal")
    , ("M-b",                        sequence_ [spawn "polybar-msg cmd toggle", sendMessage ToggleStruts])
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
    , ("M-S-r",                      sequence_ [spawn restartcmd, spawn restackcmd])
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
      term = "alacritty"
      restartcmd = "xmonad --restart && systemctl --user restart polybar"
      restackcmd = "sleep 1.2; xdo lower $(xwininfo -name polybar-xmonad | rg 'Window id' | cut -d ' ' -f4)"
      toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                then W.sink w s
                                else (W.float w (W.RationalRect 0.15 0.15 0.7 0.7) s))

  mousebindings = [ ((modkey .|. shiftMask, button1), dragWindow) ]

  scratchpads = [NS "terminal" "alacritty -t ScratchpadTerm" (title =? "ScratchpadTerm") (customFloating $ W.RationalRect 0.0 0.5 1.0 0.0)]

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

  layouts = avoidStruts $ tiled ||| mtiled ||| tabs ||| centeredMaster ||| grid
    where
       tiled = stripName 2 0 $ gaps 4 4 $ draggingVisualizer $ toggleLayouts maximized (smartBorders (Tall 1 (3/100) (1/2)))
       mtiled = stripName 2 0 $ gaps 4 4 $ draggingVisualizer $ Mirror (toggleLayouts maximized (smartBorders (Tall 1 (3/100) (1/2))))
       centeredMaster = stripName 2 0 $ gaps 4 4 $ draggingVisualizer $ toggleLayouts maximized (smartBorders (ThreeColMid 1 (3/100) (1/2)))
       tabs = stripName 1 1 $ gaps 8 0 $ noBorders (tabbed shrinkText tabTheme)
       grid = stripName 2 0 $ gaps 4 4 $ draggingVisualizer $ toggleLayouts maximized (smartBorders Grid)
       maximized = smartBorders Full
       gaps n k = spacingRaw False (Border n n n n) True (Border k k k k) True
       stripName n k = renamed [Chain [CutWordsLeft n, CutWordsRight k]]

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

  windowRules =
    placeHook (smart (0.5, 0.5))
    <+> namedScratchpadManageHook scratchpads
    <+> composeAll
    [ className  =? "Gimp"                                   --> doFloat
    , (className =? "Ripcord" <&&> title =? "Preferences")   --> doFloat
    , className  =? "Xmessage"                               --> doFloat
    , className  =? "Peek"                                   --> doFloat
    , className  =? "Xephyr"                                 --> doFloat
    , className  =? "Sxiv"                                   --> doFloat
    , className  =? "mpv"                                    --> doFloat
    , appName    =? "desktop_window"                         --> doIgnore
    , appName    =? "kdesktop"                               --> doIgnore
    , isDialog                                               --> doF siftUp <+> doFloat ]
    <+> insertPosition End Newer -- same effect as attachaside patch in dwm
    <+> manageDocks
    <+> manageHook defaultConfig

  autostart = do
    spawnOnce "xsetroot -cursor_name left_ptr &"
    spawnOnce "systemctl --user restart polybar &"
    spawnOnce "xwallpaper --zoom /etc/nixos/nixos/config/wallpapers/horizon.jpg &"
    spawnOnce "xidlehook --not-when-fullscreen --not-when-audio --timer 120 slock \'\' &"
    spawnOnce "notify-desktop -u low 'xmonad' 'started successfully'"
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

  polybarHook dbus = dynamicLogWithPP . filterOutWsPP ["NSP"] $ xmobarPP
    { ppOutput = dbusOutput dbus
    , ppOrder  = \(_:l:_:_) -> [l]
    }

  main' dbus = xmonad . ewmhFullscreen . docks . ewmh $ def
    { focusFollowsMouse  = True
    , clickJustFocuses   = True
    , borderWidth        = 2
    , modMask            = modkey
    , workspaces         = ws
    , normalBorderColor  = "#2e303e"
    , focusedBorderColor = "#e95678"
    , layoutHook         = layouts
    , manageHook         = windowRules
    , logHook            = polybarHook dbus
    , handleEventHook    = swallowEventHook (return True) (return True)
    , startupHook        = autostart
    } 
    `additionalKeysP` keybindings
    `additionalMouseBindings` mousebindings

  main = dbusClient >>= main' -- "that was easy, xmonad rocks!"
''
