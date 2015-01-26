{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Theme where

import Clckwrks
import Clckwrks.Authenticate.Plugin  (authenticatePlugin)
import Clckwrks.Authenticate.URL     (AuthURL(Auth))
import Clckwrks.Types                (NamedLink(..))
import Clckwrks.NavBar.API           (getNavBarData)
import Clckwrks.NavBar.Types         (NavBar(..), NavBarItem(..))
import Clckwrks.Monad
import Control.Monad.State           (get)
import Clckwrks.ProfileData.Acid     (HasRole(..))
import qualified Data.Set            as Set
import           Data.Text.Lazy      (Text)
import qualified Data.Text           as T
import Happstack.Authenticate.Password.URL (PasswordURL(UsernamePasswordCtrl), passwordAuthenticationMethod)
import HSP.XMLGenerator
import HSP.XML                       (XML)
import Paths_clckwrks_theme_clckwrks (getDataDir)
import Web.Plugins.Core              (pluginName, getPluginRouteFn)

theme :: Theme
theme = Theme
    { themeName      = "clckwrks"
    , themeStyles    = [standardStyle]
    , themeDataDir   = getDataDir
    }

genNavBar :: GenXML (Clck ClckURL)
genNavBar =
    do menu <- lift getNavBarData
       navBarHTML menu

navBarHTML :: NavBar -> GenXML (Clck ClckURL)
navBarHTML (NavBar menuItems) =
    <div class="navbar navbar-static-full-width">
     <div class="navbar-inner">
      <div class="container">
       <a class="brand" href="/">clckwrks</a>
       <div class="nav-collapse">
        <ul class="nav">
         <% mapM mkNavBarItem menuItems %>
        </ul>
       </div>
      </div>
     </div>
    </div>

mkNavBarItem :: NavBarItem -> GenXML (Clck ClckURL)
mkNavBarItem (NBLink (NamedLink ttl lnk)) =
    <li><a href=lnk><% ttl %></a></li>

standardTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                    , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
                    ) =>
                    T.Text
                 -> headers
                 -> body
                 -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
standardTemplate ttl hdr bdy = do
    p <- plugins <$> get
    (Just authShowURL) <- getPluginRouteFn p (pluginName authenticatePlugin)
    let passwordShowURL u = authShowURL (Auth $ AuthenticationMethods $ Just (passwordAuthenticationMethod, toPathSegments u)) []
    <html>
     <head>
      <title><% ttl %></title>
      <link rel="stylesheet" type="text/css" media="screen" href=(ThemeData "data/css/bootstrap.css")  />
      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/hscolour.css") />
      <script src="http://code.jquery.com/jquery-latest.js"></script>
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js"></script>
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular-route.min.js"></script>
      <script src=(passwordShowURL UsernamePasswordCtrl)></script>
      <script src=(JS ClckwrksApp)></script>
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <% hdr %>
      <% googleAnalytics %>
     </head>
     <body ng-app="clckwrksApp" ng-controller="AuthenticationCtrl">
      <div id="wrap">
       <% genNavBar %>
       <div class="container">
         <div class="row">
          <div class="span8">
           <% bdy %>
          </div>
         </div>
         <div id="push"></div>
       </div>
      </div>

      <footer id="footer" class="footer">
       <div class="container">
         <p class="muted">Powered by <a href="http://happstack.com/">Happstack</a> and <a href="http://clckwrks.com/">clckwrks</a>. Copyright 2013, <a href="http://seereason.com/">SeeReason Partners, LLC</a></p>
       </div>
      </footer>

     </body>
    </html>


standardStyle :: ThemeStyle
standardStyle = ThemeStyle
    { themeStyleName        = "standard"
    , themeStyleDescription = "standard view"
    , themeStylePreview     = Nothing
    , themeStyleTemplate    = standardTemplate
    }
