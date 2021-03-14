{ config, pkgs, theme }:

with pkgs;
with theme;

''
  {-# LANGUAGE FlexibleContexts #-}

  -- fortuneteller2k's XMonad config
  -- This file is managed by NixOS, don't edit it directly!

  import Data.Char
  import Data.Monoid
  import Data.Tree

  import System.IO
  import System.Exit

  import XMonad

  import XMonad.Actions.CycleWS
  import XMonad.Actions.Promote
  import XMonad.Actions.Sift
  import XMonad.Actions.TiledWindowDragging
  import XMonad.Actions.TreeSelect
  import XMonad.Actions.WithAll

  import XMonad.Hooks.DynamicLog
  import XMonad.Hooks.EwmhDesktops
  import XMonad.Hooks.InsertPosition
  import XMonad.Hooks.ManageDocks
  import XMonad.Hooks.ManageHelpers
  import XMonad.Hooks.Place
  import XMonad.Hooks.WindowSwallowing

  import XMonad.Layout.BoringWindows
  import XMonad.Layout.Decoration
  import XMonad.Layout.DraggingVisualizer
  import XMonad.Layout.Grid
  import XMonad.Layout.LayoutHints
  import XMonad.Layout.LayoutModifier
  import XMonad.Layout.Maximize
  import XMonad.Layout.NoBorders
  import XMonad.Layout.Renamed
  import XMonad.Layout.ResizableThreeColumns
  import XMonad.Layout.ResizableTile
  import XMonad.Layout.Simplest
  import XMonad.Layout.Spacing
  import XMonad.Layout.SubLayouts
  import XMonad.Layout.Tabbed
  import XMonad.Layout.WindowNavigation

  import XMonad.Prompt
  import XMonad.Prompt.FuzzyMatch
  import XMonad.Prompt.Shell

  import XMonad.Util.EZConfig
  import XMonad.Util.Hacks
  import XMonad.Util.Run
  import XMonad.Util.SpawnOnce

  import qualified Codec.Binary.UTF8.String as UTF8
  import qualified Data.Map                 as M
  import qualified DBus                     as D
  import qualified DBus.Client              as D
  import qualified XMonad.Actions.Sift      as W
  import qualified XMonad.StackSet          as W

  modkey = mod1Mask
  term = "${alacritty}/bin/alacritty"
  fontNameGTK = "Iosevka FT"
  fontFamily = "xft:" ++ fontNameGTK ++ ":size=9.7:antialias=true:hinting=true"
  sansFontFamily = "xft:Sarasa Gothic J:size=10:antialias=true:hinting=true"
  ws = [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J" ]

  actions = [ Node (TSNode "Session" "session management" (return ()))
                   [ Node (TSNode "Logout" "exit current XMonad session" (io (exitWith ExitSuccess))) []
                   , Node (TSNode "Lock" "lock session" (safeSpawnProg "${config.security.wrapperDir}/slock")) []
                   , Node (TSNode "Reboot" "reboot this machine" (safeSpawn "${systemd}/bin/systemctl" ["reboot"])) []
                   , Node (TSNode "Power Off" "power off this machine" (safeSpawn "${systemd}/bin/systemctl" ["poweroff"])) []
                   ]
            , Node (TSNode "Media" "media controls" (return ()))
                   [ Node (TSNode "MPD" "control the music player daemon" (return ()))
                          [ Node (TSNode "Play" "play current song" (safeSpawn "${mpc_cli}/bin/mpc" ["play"])) [] 
                          , Node (TSNode "Pause" "pause current song" (safeSpawn "${mpc_cli}/bin/mpc" ["pause"])) []
                          , Node (TSNode "Previous" "play previous song in playlist" (safeSpawn "${mpc_cli}/bin/mpc" ["prev"])) []
                          , Node (TSNode "Next" "play next song in playlist" (safeSpawn "${mpc_cli}/bin/mpc" ["next"])) []
                          , Node (TSNode "Repeat" "toggle repeat" (safeSpawn "${mpc_cli}/bin/mpc" ["repeat"])) []
                          , Node (TSNode "Single" "toggle single song mode" (safeSpawn "${mpc_cli}/bin/mpc" ["single"])) []
                          ]
                   , Node (TSNode "MPRIS" "control general media" (return ()))
                          [ Node (TSNode "Play" "play media" (safeSpawn "${playerctl}/bin/playerctl" ["play"])) []
                          , Node (TSNode "Pause" "pause media" (safeSpawn "${playerctl}/bin/playerctl" ["pause"])) []
                          , Node (TSNode "Previous" "go to previous" (safeSpawn "${playerctl}/bin/playerctl" ["previous"])) []
                          , Node (TSNode "Next" "go to next" (safeSpawn "${playerctl}/bin/playerctl" ["next"])) []
                          ]
                   ]
            ]

  tsConfig = TSConfig 
             { ts_hidechildren = True
             , ts_background   = 0xe0${colors.bg}
             , ts_font         = sansFontFamily
             , ts_node         = (0xff${colors.bg}, 0xff${if theme.primaryColor == "red" then colors.c1 else colors.c3})
             , ts_nodealt      = (0xff${colors.bg}, 0xff${if theme.primaryColor == "red" then colors.c9 else colors.c11})
             , ts_highlight    = (0xff${colors.bg}, 0xff${if theme.primaryColor == "red" then colors.c3 else colors.c1})
             , ts_extra        = 0xff${colors.fg}
             , ts_node_width   = 200
             , ts_node_height  = 30
             , ts_originX      = 0
             , ts_originY      = 0
             , ts_indent       = 80
             , ts_navigate     = defaultNavigation
             }

  keybindings =
    [ ("M-<Return>",                 safeSpawnProg term)
    , ("M-`",                        distractionLess)
    , ("M-d",                        shellPrompt promptConfig)
    , ("M-q",                        kill)
    , ("M-w",                        treeselectAction tsConfig actions)
    , ("M-<F2>",                     unsafeSpawn browser)
    , ("M-e",                        withFocused (sendMessage . maximizeRestore))
    , ("M-<Tab>",                    sendMessage NextLayout)
    , ("M-s",                        promote)
    , ("M--",                        sendMessage Shrink)
    , ("M-=",                        sendMessage Expand)
    , ("M-[",                        sendMessage MirrorShrink)
    , ("M-]",                        sendMessage MirrorExpand)
    , ("M-t",                        withFocused toggleFloat)
    , ("M-,",                        sendMessage (IncMasterN 1))
    , ("M-.",                        sendMessage (IncMasterN (-1)))
    , ("C-<Left>",                   prevWS)
    , ("C-<Right>",                  nextWS)
    , ("<Print>",                    safeSpawn "/etc/nixos/scripts/screenshot" ["wind"])
    , ("M-<Print>",                  safeSpawn "/etc/nixos/scripts/screenshot" ["area"])
    , ("M-S-s",                      safeSpawn "/etc/nixos/scripts/screenshot" ["full"])
    , ("M-S-q",                      io (exitWith ExitSuccess))
    , ("M-S-h",                      safeSpawn "${gxmessage}/bin/gxmessage" ["-fn", fontNameGTK, help])
    , ("M-S-<Delete>",               safeSpawnProg "${config.security.wrapperDir}/slock")
    , ("M-S-c",                      withFocused $ \w -> safeSpawn "${xorg.xkill}/bin/xkill" ["-id", show w])
    , ("M-C-<Left>",                 sendMessage $ pullGroup L)
    , ("M-C-<Right>",                sendMessage $ pullGroup R)
    , ("M-C-<Up>",                   sendMessage $ pullGroup U)
    , ("M-C-<Down>",                 sendMessage $ pullGroup D)
    , ("M-C-m",                      withFocused (sendMessage . MergeAll))
    , ("M-C-u",                      withFocused (sendMessage . UnMerge))
    , ("M-C-,",                      onGroup W.focusUp')
    , ("M-C-.",                      onGroup W.focusDown')
    , ("M-S-r",                      unsafeSpawn (restartcmd ++ "&& sleep 2 &&" ++ restackcmd))
    , ("M-S-<Left>",                 shiftToPrev >> prevWS)
    , ("M-S-<Right>",                shiftToNext >> nextWS)
    , ("M-<Left>",                   focusUp)
    , ("M-<Right>",                  focusDown)
    , ("M-S-<Tab>",                  sendMessage FirstLayout)
    , ("M-C-c",                      killAll)
    , ("<XF86AudioMute>",            safeSpawn "/etc/nixos/scripts/volume" ["toggle"])
    , ("<XF86AudioRaiseVolume>",     safeSpawn "/etc/nixos/scripts/volume" ["up"])
    , ("<XF86AudioLowerVolume>",     safeSpawn "/etc/nixos/scripts/volume" ["down"])
    , ("<XF86AudioPlay>",            safeSpawn "${playerctl}/bin/playerctl" ["play-pause"])
    , ("<XF86AudioPrev>",            safeSpawn "${playerctl}/bin/playerctl" ["previous"])
    , ("<XF86AudioNext>",            safeSpawn "${playerctl}/bin/playerctl" ["next"])
    , ("<XF86MonBrightnessUp>",      safeSpawn "${brightnessctl}/bin/brightnessctl" ["s", "+10%"])
    , ("<XF86MonBrightnessDown>",    safeSpawn "${brightnessctl}/bin/brightnessctl" ["s", "10%-"])
    ]
    ++
    [ (otherModMasks ++ "M-" ++ key, action tag)
        | (tag, key) <- zip ws (map show ([1..9] ++ [0]))
        , (otherModMasks, action) <- [ ("", windows . W.greedyView)
                                     , ("S-", windows . W.shift) ] ]
    where 
      distractionLess = sequence_ [unsafeSpawn restackcmd, sendMessage ToggleStruts, toggleScreenSpacingEnabled, toggleWindowSpacingEnabled]
      restartcmd = "${xmonad-with-packages}/bin/xmonad --restart && ${polybar}/bin/polybar-msg cmd restart"
      restackcmd = "${xdo}/bin/xdo lower $(${xorg.xwininfo}/bin/xwininfo -name polybar-xmonad | ${ripgrep}/bin/rg 'Window id' | ${coreutils}/bin/cut -d ' ' -f4)"
      browser = concat
        [ "${qutebrowser}/bin/qutebrowser"
        , " --qt-flag ignore-gpu-blacklist"
        , " --qt-flag enable-gpu-rasterization"
        , " --qt-flag enable-native-gpu-memory-buffers"
        , " --qt-flag num-raster-threads=4"
        , " --qt-flag enable-oop-rasterization" ]
      toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                      then W.sink w s
                                      else (W.float w (W.RationalRect 0.15 0.15 0.7 0.7) s))
      promptConfig = def
        { font                = fontFamily
        , bgColor             = "#${colors.bg}"
        , fgColor             = "#${colors.fg}"
        , bgHLight            = "#${colors.highlightColor}"
        , fgHLight            = "#${colors.bg}"
        , promptBorderWidth   = 0
        , position            = Top
        , height              = 17
        , historySize         = 256
        , historyFilter       = id
        , showCompletionOnTab = False
        , searchPredicate     = fuzzyMatch
        , sorter              = fuzzySort
        , defaultPrompter     = \_ -> "xmonad λ: "
        , alwaysHighlight     = True
        , maxComplRows        = Just 5
        }

  mousebindings = 
    [ ((modkey .|. shiftMask, button1), dragWindow)
    , ((modkey, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modkey, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)) ]

  layouts = avoidStruts 
            $ renamed [CutWordsLeft 5]
            $ smartBorders
            $ configurableNavigation noNavigateBorders
            $ tabs
            $ boringWindows
            $ spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
            $ draggingVisualizer
            $ maximizeWithPadding 0
            $ layoutHints
            $ (tall ||| Mirror tall ||| threecol ||| Grid)
    where
      tall = ResizableTall 1 (3/100) (11/20) []
      threecol = ResizableThreeColMid 1 (3/100) (1/2) []
      tabs x = addTabs shrinkText tabTheme $ subLayout [] Simplest x
      tabTheme = def
        { fontName            = fontFamily
        , activeColor         = "#${colors.activeBorderColor}"
        , inactiveColor       = "#${colors.inactiveBorderColor}"
        , urgentColor         = "#${colors.c5}"
        , activeTextColor     = "#${colors.bg}"
        , inactiveTextColor   = "#${colors.fg}"
        , urgentTextColor     = "#${colors.bg}"
        , activeBorderColor   = "#${colors.activeBorderColor}"
        , inactiveBorderColor = "#${colors.inactiveBorderColor}"
        , urgentBorderColor   = "#${colors.c3}"
        , activeBorderWidth   = 2
        , inactiveBorderWidth = 2
        , urgentBorderWidth   = 2
        }

  windowRules =
    placeHook (smart (0.5, 0.5))
    <+> composeAll
    [ className  =? "Gimp"                                 --> doFloat
    , (className =? "Ripcord" <&&> title =? "Preferences") --> doFloat
    , className  =? "Gxmessage"                            --> doFloat
    , className  =? "Peek"                                 --> doFloat
    , className  =? "Xephyr"                               --> doFloat
    , className  =? "Sxiv"                                 --> doFloat
    , appName    =? "polybar"                              --> doLower
    , appName    =? "desktop_window"                       --> doIgnore
    , appName    =? "kdesktop"                             --> doIgnore
    , isDialog                                             --> doF W.siftUp <+> doFloat ]
    <+> insertPosition End Newer -- same effect as attachaside patch in dwm
    <+> manageDocks
    <+> manageHook defaultConfig
    where
      doLower = ask >>= \w -> unsafeSpawn ("${xdo}/bin/xdo lower " ++ show w) >> mempty

  autostart = do
    spawnOnce "${xwallpaper}/bin/xwallpaper --zoom ${wallpaper} &"
    spawnOnce "${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &"
    spawnOnce "${xidlehook}/bin/xidlehook --not-when-fullscreen --not-when-audio --timer 120 slock \'\' &"
    spawnOnce "${polybar}/bin/polybar-msg cmd restart &"
    spawnOnce "${notify-desktop}/bin/notify-desktop -u critical 'xmonad' 'started successfully'"

  barHook dbus =
    let signal     = D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")
        output str = D.emit dbus $ signal { D.signalBody = [ D.toVariant $ UTF8.decodeString str ] } 
    in dynamicLogWithPP $ xmobarPP
      { ppOutput = output
      , ppOrder  = \(_:l:_:_) -> [l]
      }

  main' dbus = xmonad . ewmhFullscreen . docks . ewmh . javaHack $ def
    { focusFollowsMouse  = True
    , clickJustFocuses   = True
    , borderWidth        = 2
    , modMask            = modkey
    , workspaces         = ws
    , normalBorderColor  = "#${colors.inactiveBorderColor}"
    , focusedBorderColor = "#${colors.activeBorderColor}"
    , layoutHook         = layouts
    , manageHook         = windowRules
    , logHook            = barHook dbus
    , handleEventHook    = swallowEventHook (className =? "Alacritty" <||> className =? "XTerm") (return True) <+> hintsEventHook
    , startupHook        = autostart
    } 
    `additionalKeysP` keybindings
    `additionalMouseBindings` mousebindings

  main = do
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.log") [ D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue ]
    main' dbus -- "that was easy, xmonad rocks!"

  help = unlines 
    [ "fortuneteller2k's XMonad configuration"
    , ""
    , "Default keybindings:"
    , ""
    , "Alt-Enter:              spawn alacritty"
    , "Alt-`:                  toggle distraction less mode"
    , "Alt-d:                  show run prompt"
    , "Alt-q:                  quit window"
    , "Alt-w:                  open treeselect for actions"
    , "Alt-F2:                 spawn qutebrowser"
    , "Alt-e:                  maximize window"
    , "Alt-Tab:                cycle through layouts"
    , "Alt-s:                  swap focused window with master window"
    , "Alt-minus:              shrink window"
    , "Alt-=:                  expand window"
    , "Alt-[:                  shrink window mirrored"
    , "Alt-]:                  expand window mirrored"
    , "Alt-t:                  toggle floating of window"
    , "Alt-,:                  increase number of master windows"
    , "Alt-.:                  decrease number of master windows"
    , "Alt-Left:               focus previous window"
    , "Alt-Right:              focus next window"
    , "Alt-LeftClick:          float window and drag it with cursor"
    , "Alt-RightClick:         float window and resize it"
    , "Alt-[0-9]:              for [1-9] go to nth workspace, for 0 go to 10th workspace"
    , "Alt-Print:              take screenshot of focused window and copy to clipboard"
    , "Alt-Shift-s:            take screenshot of whole screen and save it to a file"
    , "Alt-Shift-q:            exit xmonad"
    , "Alt-Shift-c:            force quit window"
    , "Alt-Shift-Delete:       lock screen"
    , "Alt-Shift-h:            show this help window"
    , "Alt-Shift-r:            restart xmonad and polybar"
    , "Alt-Shift-Left:         move window to previous workspace and focus that workspace"
    , "Alt-Shift-Right:        move window to next workspace and focus that workspace"
    , "Alt-Shift-Tab:          reset layout to Tall (master and stack)"
    , "Alt-Shift-LeftClick:    move window to dragged position"
    , "Alt-Ctrl-c:             kill all windows in workspace"
    , "Alt-Ctrl-Left:          group focused window to it's left"
    , "Alt-Ctrl-Right:         group focused window to it's right"
    , "Alt-Ctrl-Up:            group focused window to it's up"
    , "Alt-Ctrl-Down:          group focused window to it's down"
    , "Alt-Ctrl-m:             group all windows in workspace"
    , "Alt-Ctrl-u:             ungroup focused window" 
    , "Alt-Ctrl-,:             focus previous window in group"
    , "Alt-Ctrl-.:             focus next window in group"
    , "Ctrl-Left:              focus previous workspace"
    , "Ctrl-Right:             focus next workspace"
    , "XF86AudioMute:          mute audio"
    , "XF86AudioPlay:          toggle play/pause"
    , "XF86AudioPrev:          go to previous"
    , "XF86AudioNext:          go to next"
    , "XF86AudioRaiseVolume:   raise volume by 10%"
    , "XF86AudioLowerVolume:   lower volume by 10%"
    , "XF86MonBrightnessUp:    raise brightness by 10%"
    , "XF86MonBrightnessDown:  lower brightness by 10%"
    ]
''
