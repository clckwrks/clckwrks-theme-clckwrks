{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme where

import Clckwrks
import Clckwrks.Monad
import Data.Text (Text)
import Happstack.Server
import HSP

theme :: Theme
theme = Theme
    { themeName      = "bootstrap-theme"
    , _themeTemplate = pageTemplate
    }

pageTemplate :: XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
pageTemplate =
    <html>
     <head>
      <title><% getPageTitle %></title>
      <link href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.1.1/css/bootstrap.min.css"        rel="stylesheet" media="screen" />
      <link href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.1.1/css/bootstrap-responsive.css" rel="stylesheet" />
      <link rel="stylesheet" type="text/css" href=(ThemeData "style.css") />
      <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <% googleAnalytics %>
     </head>
     <body>
      <div class="page-menu">
       <a href="/" id="menu-logo">clckwrks.com</a>
       <div class="menu-inner-div">
        <% getMenu %>
      <% do mu <- getUserId
            case mu of
              Nothing  -> <span id="login-link"><a href=(Auth $ AuthURL A_Login)>Login</a></span>
              (Just _) -> <span id="login-link"><a href=(Auth $ AuthURL A_Logout)>Logout</a></span>
       %>

       </div>

      </div>

      <div class="container">
       <div class="row">
        <h1><% getPageTitle %></h1>
        <% getPageContent %>
       </div>
      </div>
      <script src="http://code.jquery.com/jquery-latest.js"></script>
     </body>
    </html>
