{ config, pkgs, theme }:

with pkgs;
with theme;

let
  xwallpaperFlag = if colors.tiledWallpaper then "--tile" else "--zoom";
in
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
  import XMonad.Hooks.UrgencyHook
  import XMonad.Hooks.WindowSwallowing

  import XMonad.Layout.BinarySpacePartition
  import XMonad.Layout.BoringWindows
  import XMonad.Layout.Decoration
  import XMonad.Layout.DraggingVisualizer
  import XMonad.Layout.Grid
  import XMonad.Layout.LayoutHints
  import XMonad.Layout.LayoutModifier
  import XMonad.Layout.Maximize
  import XMonad.Layout.MultiToggle
  import XMonad.Layout.MultiToggle.Instances
  import XMonad.Layout.NoBorders
  import XMonad.Layout.Renamed
  import XMonad.Layout.ResizableThreeColumns
  import XMonad.Layout.ResizableTile
  import XMonad.Layout.Simplest
  import XMonad.Layout.Spacing
  import XMonad.Layout.SubLayouts
  import XMonad.Layout.Tabbed
  import XMonad.Layout.WindowNavigation hiding (Swap)

  import XMonad.Prompt
  import XMonad.Prompt.FuzzyMatch
  import XMonad.Prompt.Shell

  import XMonad.Util.EZConfig
  import XMonad.Util.Hacks
  import XMonad.Util.PureX
  import XMonad.Util.Run
  import XMonad.Util.SpawnOnce

  import qualified Codec.Binary.UTF8.String as UTF8
  import qualified Data.Map                 as M
  import qualified DBus                     as D
  import qualified DBus.Client              as D
  import qualified XMonad.Actions.Sift      as W
  import qualified XMonad.StackSet          as W

  fontNameGTK = "Iosevka FT"
  fontFamily = "xft:" ++ fontNameGTK ++ ":size=9.7:antialias=true:hinting=true"
  sansFontFamily = "xft:Sarasa Gothic J:size=10:antialias=true:hinting=true"
  ws = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

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
             , ts_node         = (0xff${colors.bg}, 0xff${colors.primary})
             , ts_nodealt      = (0xff${colors.bg}, 0xff${colors.primaryBright})
             , ts_highlight    = (0xff${colors.bg}, 0xff${colors.secondary})
             , ts_extra        = 0xff${colors.fg}
             , ts_node_width   = 200
             , ts_node_height  = 30
             , ts_originX      = 0
             , ts_originY      = 0
             , ts_indent       = 80
             , ts_navigate     = defaultNavigation
             }

  keybindings = \c -> mkKeymap c $
    [ ("M-<Return>",                 safeSpawnProg $ terminal c)
    , ("M-`",                        distractionLess)
    , ("M-d",                        shellPrompt promptConfig)
    , ("M-q",                        kill)
    , ("M-w",                        treeselectAction tsConfig actions)
    , ("M-<F2>",                     unsafeSpawn qutebrowser)
    , ("M-e",                        withFocused (sendMessage . maximizeRestore))
    , ("M-<Tab>",                    sendMessage NextLayout)
    , ("M-s",                        promote)
    , ("M--",                        sendMessage Shrink)
    , ("M-=",                        sendMessage Expand)
    , ("M-[",                        sendMessage MirrorExpand)
    , ("M-]",                        sendMessage MirrorShrink)
    , ("M-t",                        withFocused toggleFloat)
    , ("M-,",                        sendMessage (IncMasterN 1))
    , ("M-.",                        sendMessage (IncMasterN (-1)))
    , ("M-;",                        sequence_ [incScreenSpacing 2, incWindowSpacing 2])
    , ("M-'",                        sequence_ [decScreenSpacing 2, decWindowSpacing 2])
    , ("M4-`",                       focusUrgent)
    , ("M4-<Esc>",                   clearUrgents)
    , ("C-<Left>",                   prevWS)
    , ("C-<Right>",                  nextWS)
    , ("<Print>",                    safeSpawn "/home/fortuneteller2k/.local/bin/screenshot" ["wind"])
    , ("M-<Print>",                  safeSpawn "/home/fortuneteller2k/.local/bin/screenshot" ["area"])
    , ("M4-<Print>",                 safeSpawn "/home/fortuneteller2k/.local/bin/screenshot" ["full"])
    , ("M-S-q",                      io (exitWith ExitSuccess))
    , ("M-S-h",                      safeSpawn "${gxmessage}/bin/gxmessage" ["-fn", fontNameGTK, help])
    , ("M-S-<Delete>",               safeSpawnProg "${config.security.wrapperDir}/slock")
    , ("M-S-c",                      withFocused $ \w -> safeSpawn "${xorg.xkill}/bin/xkill" ["-id", show w])
    , ("M4-<L>",                     sendMessage $ pullGroup L)
    , ("M4-<R>",                     sendMessage $ pullGroup R)
    , ("M4-<U>",                     sendMessage $ pullGroup U)
    , ("M4-<D>",                     sendMessage $ pullGroup D)
    , ("M4-m",                       withFocused (sendMessage . MergeAll))
    , ("M4-u",                       withFocused (sendMessage . UnMerge))
    , ("M4-,",                       onGroup W.focusUp')
    , ("M4-.",                       onGroup W.focusDown')
    , ("M-S-r",                      safeSpawn "${xmonad-with-packages}/bin/xmonad" ["--restart"])
    , ("M-S-<Left>",                 shiftToPrev >> prevWS)
    , ("M-S-<Right>",                shiftToNext >> nextWS)
    , ("M-<Left>",                   focusUp)
    , ("M-<Right>",                  focusDown)
    , ("M4-<Tab>",                   resetLayout c)
    , ("M4-q",                       killAll)
    , ("M-M4-<Left>",                sendMessage $ ExpandTowards L)
    , ("M-M4-<Right>",               sendMessage $ ShrinkFrom L)
    , ("M-M4-<Up>",                  sendMessage $ ExpandTowards U)
    , ("M-M4-<Down>",                sendMessage $ ShrinkFrom U)
    , ("M-M4-C-<Left>",              sendMessage $ ShrinkFrom R)
    , ("M-M4-C-<Right>",             sendMessage $ ExpandTowards R)
    , ("M-M4-C-<Up>",                sendMessage $ ShrinkFrom D)
    , ("M-M4-C-<Down>",              sendMessage $ ExpandTowards D)
    , ("M4-s",                       sendMessage Swap)
    , ("M-M4-s",                     sendMessage Rotate)
    , ("M4-j",                       sendMessage $ SplitShift Prev)
    , ("M4-k",                       sendMessage $ SplitShift Next)
    , ("<XF86AudioMute>",            safeSpawn "/home/fortuneteller2k/.local/bin/volume" ["toggle"])
    , ("<XF86AudioRaiseVolume>",     safeSpawn "/home/fortuneteller2k/.local/bin/volume" ["up"])
    , ("<XF86AudioLowerVolume>",     safeSpawn "/home/fortuneteller2k/.local/bin/volume" ["down"])
    , ("<XF86AudioPlay>",            safeSpawn "${playerctl}/bin/playerctl" ["play-pause"])
    , ("<XF86AudioPrev>",            safeSpawn "${playerctl}/bin/playerctl" ["previous"])
    , ("<XF86AudioNext>",            safeSpawn "${playerctl}/bin/playerctl" ["next"])
    , ("<XF86MonBrightnessUp>",      safeSpawn "${brightnessctl}/bin/brightnessctl" ["s", "+10%"])
    , ("<XF86MonBrightnessDown>",    safeSpawn "${brightnessctl}/bin/brightnessctl" ["s", "10%-"])
    ]
    ++
    [ (otherModMasks ++ "M-" ++ key, action tag)
        | (tag, key) <- zip ws (map show ([1..9] ++ [0]))
        , (otherModMasks, action) <- [("", windows . W.greedyView), ("S-", windows . W.shift)] ]
    where 
      qutebrowser = concat
        [ "${qutebrowser}/bin/qutebrowser"
        , " --qt-flag ignore-gpu-blacklist"
        , " --qt-flag enable-gpu-rasterization"
        , " --qt-flag enable-native-gpu-memory-buffers"
        , " --qt-flag num-raster-threads=4"
        , " --qt-flag enable-oop-rasterization"
        ]
      distractionLess = handlingRefresh $ sequence_
        [ safeSpawn "${polybar}/bin/polybar-msg" ["cmd", "toggle"]
        , broadcastMessage ToggleStruts
        , broadcastMessage (ModifyScreenBorderEnabled not)
        , broadcastMessage (ModifyWindowBorderEnabled not)
        , broadcastMessage $ Toggle NOBORDERS
        ]
      resetLayout conf = handlingRefresh $ sequence_
        [ broadcastMessage $ SetStruts [minBound .. maxBound] []
        , broadcastMessage (ModifyScreenBorderEnabled (return True))
        , broadcastMessage (ModifyWindowBorderEnabled (return True))
        , safeSpawn "${polybar}/bin/polybar-msg" ["cmd", "show"]
        , setAllLayout $ layoutHook conf
        ]
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
    [ ((mod4Mask, button1), dragWindow)
    , ((mod1Mask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((mod1Mask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

  -- full credit to u/jwofejofwjedf
  setAllLayout l = do
      ss@W.StackSet { W.current = c@W.Screen { W.workspace = ws }, W.visible = sVisible, W.hidden = sHidden } <- gets windowset
      handleMessage (W.layout ws) (SomeMessage ReleaseResources)
      windows $ const $ ss { W.current = c { W.workspace = ws { W.layout = l } }, W.visible = setSL sVisible, W.hidden = setHL sHidden }
    where
      setHL s = map (\w -> w { W.layout = l }) s
      setSL = map (\w -> w { W.workspace = (W.workspace w) { W.layout = l } })

  layouts = avoidStruts
            . renamed [CutWordsLeft 5]
            . smartBorders
            . mkToggle1 NOBORDERS
            . configurableNavigation noNavigateBorders
            . tabs
            . boringWindows
            . spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
            . draggingVisualizer
            . maximizeWithPadding 0
            . layoutHints
            $ (tall ||| Mirror tall ||| emptyBSP ||| threecol ||| Grid)
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
        , activeBorderWidth   = ${theme.borderWidth}
        , inactiveBorderWidth = ${theme.borderWidth}
        , urgentBorderWidth   = ${theme.borderWidth}
        }

  windowRules = composeAll
    [ placeHook (smart (0.5, 0.5))
    , className  =? "Gimp"                                 --> doFloat
    , (className =? "Ripcord" <&&> title =? "Preferences") --> doFloat
    , className  =? "Gxmessage"                            --> doFloat
    , className  =? "Peek"                                 --> doFloat
    , className  =? "Xephyr"                               --> doFloat
    , className  =? "Sxiv"                                 --> doFloat
    , appName    =? "desktop_window"                       --> doIgnore
    , appName    =? "kdesktop"                             --> doIgnore
    , isDialog                                             --> doF W.siftUp <+> doFloat
    , insertPosition End Newer -- same effect as attachaside patch in dwm
    , manageDocks
    , manageHook defaultConfig
    ]

  autostart = do
    spawnOnce "${xwallpaper}/bin/xwallpaper ${xwallpaperFlag} ${colors.wallpaper} &"
    spawnOnce "${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &"
    spawnOnce "${polybar}/bin/polybar-msg cmd restart &"
    spawnOnce "${notify-desktop}/bin/notify-desktop -u critical 'xmonad' 'started successfully'"

  barHook dbus =
    let signal     = D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")
        output str = D.emit dbus $ signal { D.signalBody = [ D.toVariant $ UTF8.decodeString str ] } 
    in dynamicLogWithPP $ xmobarPP
      { ppOutput = output
      , ppOrder  = \(_:l:_:_) -> [l]
      }

  xmonadConfig dbus = def
    { terminal           = "${wezterm}/bin/wezterm"
    , focusFollowsMouse  = True
    , clickJustFocuses   = True
    , borderWidth        = ${theme.borderWidth}
    , keys               = keybindings
    , modMask            = mod1Mask
    , workspaces         = ws
    , normalBorderColor  = "#${colors.inactiveBorderColor}"
    , focusedBorderColor = "#${colors.activeBorderColor}"
    , layoutHook         = layouts
    , manageHook         = windowRules
    , logHook            = barHook dbus
    , handleEventHook    = swallowEventHook swallowExclude (return True) <+> hintsEventHook
    , startupHook        = autostart
    } `additionalMouseBindings` mousebindings
    where
      swallowExclude = className =? "Alacritty" <||> className =? "org.wezfurlong.wezterm" <||> className =? "XTerm"

  main =
    let
      borderUrgHook = BorderUrgencyHook { urgencyBorderColor = "#${colors.c1}" }
      urgConfig = urgencyConfig { suppressWhen = XMonad.Hooks.UrgencyHook.Never }
    in do
      dbus <- D.connectSession
      D.requestName dbus (D.busName_ "org.xmonad.log") [ D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue ]
      xmonad . ewmhFullscreen . docks . ewmh . javaHack . withUrgencyHookC borderUrgHook urgConfig $ xmonadConfig dbus

  help = unlines 
    [ "fortuneteller2k's XMonad configuration"
    , ""
    , "Default keybindings:"
    , ""
    , "Alt-Enter:              spawn terminal"
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
    , "Alt-;:                  increase gap size by 2"
    , "Alt-':                  decrease gap size by 2"
    , "Alt-Left:               focus previous window"
    , "Alt-Right:              focus next window"
    , "Alt-LeftClick:          float window and drag it with cursor"
    , "Alt-RightClick:         float window and resize it"
    , "Alt-[0-9]:              for [1-9] go to nth workspace, for 0 go to 10th workspace"
    , "Alt-Print:              take screenshot of focused window and copy to clipboard"
    , "Super-Print:            take screenshot of whole screen and save it to a file"
    , "Alt-Shift-q:            exit xmonad"
    , "Alt-Shift-c:            force quit window"
    , "Alt-Shift-Delete:       lock screen"
    , "Alt-Shift-h:            show this help window"
    , "Alt-Shift-r:            restart xmonad and polybar"
    , "Alt-Shift-Left:         move window to previous workspace and focus that workspace"
    , "Alt-Shift-Right:        move window to next workspace and focus that workspace"
    , "Super-`:                focus recently urgent window"
    , "Super-Escape:           clear urgents"
    , "Super-Tab:              reset layout to default"
    , "Super-LeftClick:        move window to dragged position"
    , "Super-q:                kill all windows in workspace"
    , "Super-Left:             group focused window to it's left"
    , "Super-Right:            group focused window to it's right"
    , "Super-Up:               group focused window to it's up"
    , "Super-Down:             group focused window to it's down"
    , "Super-m:                group all windows in workspace"
    , "Super-u:                ungroup focused window"
    , "Super-,:                focus previous window in group"
    , "Super-.:                focus next window in group"
    , "Super-s:                BSP: swap window positions"
    , "Super-j:                BSP: split previous"
    , "Super-k:                BSP: split next"
    , "Alt-Super-Left:         BSP: expand towards left"
    , "Alt-Super-Right:        BSP: shrink from left"
    , "Alt-Super-Up:           BSP: expand upwards"
    , "Alt-Super-Down:         BSP: shrink from above"
    , "Alt-Super-Ctrl-Left:    BSP: shrink from the right"
    , "Alt-Super-Ctrl-Right:   BSP: expand to the right"
    , "Alt-Super-Ctrl-Up:      BSP: shrink from below"
    , "Alt-Super-Ctrl-Down:    BSP: expand downwards"
    , "Alt-Super-s:            BSP: rotate"
    , "Ctrl-Left:              focus previous workspace"
    , "Ctrl-Right:             focus next workspace"
    , "XF86AudioMute:          mute audio sink"
    , "XF86AudioPlay:          toggle play/pause"
    , "XF86AudioPrev:          go to previous"
    , "XF86AudioNext:          go to next"
    , "XF86AudioRaiseVolume:   raise volume by 10%"
    , "XF86AudioLowerVolume:   lower volume by 10%"
    , "XF86MonBrightnessUp:    raise brightness by 10%"
    , "XF86MonBrightnessDown:  lower brightness by 10%"
    ]
''
