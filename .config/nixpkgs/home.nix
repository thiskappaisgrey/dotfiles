{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "thanawat";
  home.homeDirectory = "/home/thanawat";
  fonts.fontconfig.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
  programs.emacs = { enable = true; };
  # TODO Move over the Xmonad config to home-manager
  # TODO add gtk config with home-manager
  gtk.iconTheme = {
    package = pkgs.gnome3.adwaita-icon-theme;
    name = "Adwaita";
  };
  # TODO manage doom emacs using home manager too?????
  home.packages = with pkgs; [
    akira-unstable
    gnome3.adwaita-icon-theme
    ardour
    audacity
    cachix
    cmus
    conky
    element-desktop
    exercism
    feh
    ffmpeg
    file
    fontpreview
    fzf
    gimp
    gnumake
    imagemagick
    languagetool
    libqalculate
    libreoffice
    lingot
    lmms
    lsd # next gen ls command
    mozjpeg
    mp3info
    mypaint
    obs-studio
    pandoc
    pcmanfm # file manager

    poppler_utils # for pdf stuff

    rlwrap # for wrapping sqlite..

    signal-desktop
    starship
    sxiv
    tldr
    tmux
    usbutils
    wmctrl
    xclip
    xorg.xmodmap
    xournalpp
    xorg.xwininfo
    youtube-dl
    ytfzf
    ytmdl
    zathura
    zotero

    scrot # screenshots
    slop # better screen selectoin
    niv
    guitarix

    alsa-utils # for volume control
    pasystray

    tlwg
    caffeine-ng
    freetube
    anki
    nixfmt
    ripgrep
    fd
    sqlite
    graphviz
    tldr
    taffybar
    haskellPackages.status-notifier-item
  ];
}
