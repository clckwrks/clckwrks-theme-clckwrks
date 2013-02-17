{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme where

import Clckwrks
import Clckwrks.Types        (NamedLink(..))
import Clckwrks.NavBar.API   (getNavBarData)
import Clckwrks.NavBar.Types (NavBar(..), NavBarItem(..))
import Clckwrks.Monad
import Clckwrks.ProfileData.Acid (HasRole(..))
import qualified Data.Set        as Set
import Data.Text (Text)
import HSP
import Paths_clckwrks_theme_clckwrks (getDataDir)

theme :: Theme
theme = Theme
    { themeName      = "clckwrks"
    , _themeTemplate = pageTemplate
    , themeDataDir   = getDataDir
    }

pageTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
                ) =>
                Text
             -> headers
             -> body
             -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
pageTemplate ttl hdr bdy = standardTemplate ttl hdr bdy
{-
    do pid <- XMLGenT $ getPageId
       case pid of
         (PageId 1) -> home ttl hdr bdy
         _          -> standardTemplate ttl hdr <div id="page-content">
                                                 <h1 class="page-title"><% ttl %></h1>
                                                 <% bdy %>
                                                </div>
-}


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
                    Text
                 -> headers
                 -> body
                 -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
standardTemplate ttl hdr bdy =
    <html>
     <head>
      <title><% ttl %></title>
--      <link href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.1.1/css/bootstrap.min.css"        rel="stylesheet" media="screen" />
--      <link href="//bootswatch.com/simplex/bootstrap.min.css"        rel="stylesheet" media="screen" />
--      <link href="//bootswatch.com/spacelab/bootstrap.min.css" rel="stylesheet" media="screen" />
--      <link href="//bootswatch.com/css/bootswatch.css"         rel="stylesheet" media="screen" />
--      <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.1.1/css/bootstrap-responsive.css" rel="stylesheet" />
      <link rel="stylesheet" type="text/css" media="screen" href=(ThemeData "bootstrap.css")  />
--      <link rel="stylesheet" type="text/css" media="screen" href=(ThemeData "bootstrap-responsive.min.css") />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <script src="http://code.jquery.com/jquery-latest.js"></script>
      <% hdr %>
      <% googleAnalytics %>
     </head>
     <body>
      <% genNavBar %>
      <div class="container">
        <div class="row">
         <div class="span8">
          <h1><% ttl %></h1>
         </div>
        </div>
        <div class="row">
         <div class="span8">
          <% bdy %>
         </div>
        </div>
      </div>
{-
      <div class="page-menu">
       <a href="/" id="menu-logo">clckwrks.com</a>
       <div class="menu-inner-div">
        <% getNavBar %>
      <% do mu <- getUserId
            case mu of
              Nothing  -> <span id="login-link"><a href=(Auth $ AuthURL A_Login)>Login</a></span>
              (Just _) -> <span id="login-link"><a href=(Auth $ AuthURL A_Logout)>Logout</a></span>
       %>

       </div>

      </div>

--      <div class="container">
--       <div class="row">
--        <h1><% getPageTitle %></h1>
        <% bdy %>
  --      <% getPageContent %>
--       </div>
--      </div>-
-}
     </body>
    </html>

------------------------------------------------------------------------------
-- blog
------------------------------------------------------------------------------
{-
postsHTML :: XMLGenT (Clck ClckURL) XML
postsHTML =
    do posts <- getPosts
       <ol class="blog-posts">
        <% mapM postHTML posts %>
        </ol>

postHTML :: Page -> XMLGenT (Clck ClckURL) XML
postHTML Page{..} =
    <li class="blog-post">
     <h2><% pageTitle %></h2>
     <span class="pub-date"><% pageDate %></span>
     <% pageSrc %>
     <p><a href=(ViewPage pageId)>permalink</a></p>
    </li>

blog :: XMLGenT (Clck ClckURL) XML
blog =
    do ttl <- lift getBlogTitle
       standardTemplate ttl () $
           <%>
            <div id="blog-content">
             <h1 class="page-title"><% ttl %></h1>
             <% postsHTML %>
            </div>
           </%>
-}
------------------------------------------------------------------------------
-- home
------------------------------------------------------------------------------
{-
summaryBox :: PageId -> String -> String -> GenXML (Clck ClckURL)
summaryBox pid title iconURL =
    <div class="summary-box">
     <h2><% title %></h2>
--     <img src=(ThemeData iconURL) />
     <% getPageSummary pid %>
     <p class="read-more"><a href=(ViewPage pid)>read more...</a></p>
    </div>

-- home :: XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
home :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
        , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
        ) =>
        Text
     -> headers
     -> body
     -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
home ttl hdr bdy =
    standardTemplate "clckwrks.com" hdr $
        <div id="homepage">

         <div id="page-content">
         <h1>clckwrks</h1>
{-
         <div id="logo">
          <img src=(ThemeData "clckwrks-logo.png") />
         </div>
-}
         <blockquote>
--          <p><span class="big-quote">“</span>runs smoothly and invisibly<span class="big-quote">”</span> - <span class="quote-author">Katherine Durkes</span></p>
          <p><span class="big-quote">“</span>An open-source CMS you can trust, built with the dexterity of Haskell.<span class="big-quote">”</span></p>
         </blockquote>

          <% bdy %>

          <div class="summary-boxes">
           <% summaryBox (PageId 8) "Why?" "philosophy-icon.png" %>
           <% summaryBox (PageId 3) "Get Started" "7-icon.png" %>
           <% summaryBox (PageId 2) "Get Involved" "8-icon.png" %>
         </div>

         </div>

        </div>
-}