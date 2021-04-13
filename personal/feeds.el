(setq elfeed-feeds
	  '(
		;; Myself
		("https://aabm.neocities.org/index.xml" blog myself left) ;; aabm.neocities.org

		;; Left
		("https://thenextrecession.wordpress.com/feed/" blog left economics english first) ;; Michael Roberts blog
		("https://paulcockshott.wordpress.com/feed/" blog left economics english first) ;; Paul Cockshott's blog
		("http://paulcockshott.co.uk/feed/" blog left economics english first) ;; Paul Cockshott
		("https://blogdaboitempo.com.br/feed/" blog brasil left português first) ;; Blog da Boitempo
		("https://cepsongunbr.com/feed/" blog left português first) ;; Centro de Estudos da Política Songun Brasil
		("https://pcb.org.br/portal2/feed/" blog news left org português first) ;; PCB
		("https://newmultitude.org/feed/" blog news left imageboard first) ;; New Multitude (/leftypol/ online magazine)
		("https://lavrapalavra.com/feed/" blog left português psychonalysis first) ;; LavraPalavra
		("https://medium.com/feed/aopovobrasileiro" blog left português brasil org mlm first) ;; GEAPB
		("https://monthlyreview.org/feed/" magazine left english first) ;; Monthly Review
		("https://realismomarxista.wordpress.com/feed" blog left social-science português first) ;; Realismo Marxista
		("https://ianwrightsite.wordpress.com/feed" blog left economics esoteric first) ;; Dark Marxism - Ian P. Wright
		
		;; News
		("https://feeds.folha.uol.com.br/poder/rss091.xml" news brasil português second) ;; Folha de SP
		("https://theintercept.com/feed/?lang=pt" news brasil world português second) ;; The Intercept Brasil
		("https://revistaopera.com.br/feed/" news left brasil português first) ;; Revista Ópera
		("https://asiatimes.com/feed" news asia english second) ;; Asia Times
		("https://www.celag.org/feed" news latam español second) ;; Centro Estrategico Latinoamericano de Geopolítica (CELAG)
		("https://lab.org.uk/feed" news latam english second) ;; Latin America Bureau
		("https://www.middleeasteye.net/rss" news mena english second) ;; Middle East Eye
		("https://feedfry.com/rss/11eb34cce6b8704ca089d0fffedaeef1" news world warez english first) ;; VK: What's News
		("https://www.sixthtone.com/rss" news china english second) ;; Sixth Tone

		;; Tech
		("https://planet.gnu.org/rss20.xml" tech freesoftware second) ;; Planet GNU
		("https://nyxt.atlas.engineer/feed" tech devel emacs second) ;; Nyxt Browser
		("https://element.io/blog/feed" tech devel privacy second) ;; Element blog
		("https://www.fsf.org/static/fsforg/rss/news.xml" tech freesoftware second) ;; FSF news
		("https://www.archlinux.org/feeds/news/" tech arch distro second)

		;; Imageboards
		("https://bunkerchan.xyz/tech/index.rss" imageboard tech second) ;; Bunkerchan /tech/
		("https://lainchan.org/sec/index.rss" imageboard tech second) ;; Lainchan /sec/
		("https://lainchan.org/Ω/index.rss" imageboard tech second) ;; Lainchan /tech/
		("https://sushigirl.us/silicon/index.rss" imageboard tech second) ;; SushiChan /silicon/
		;; Videos
		;; Left
 		("https://youtube.com/feeds/videos.xml?channel_id=UCSm1_XO-zvR0ToSJYMljmPA" videos left shitpost imageboard first) ;; /leftypol/ videos
		("https://youtube.com/feeds/videos.xml?channel_id=UCl1bDSVi34xE65YzreytVxQ" videos left first) ;; Anticonquista
		("https://youtube.com/feeds/videos.xml?channel_id=UCu5O6b1IynT40gGtn787A_g" videos left shitpost imageboard second) ;; GETchan
		("https://youtube.com/feeds/videos.xml?channel_id=UCPPZoYsfoSekIpLcz9plX1Q" videos left first) ;; Hakim
		("https://youtube.com/feeds/videos.xml?channel_id=UC02coXfDPjEmU8uDT2G8Z2A" videos left brasil second) ;; Jones Manoel
		("https://youtube.com/feeds/videos.xml?channel_id=UC7-Pp09PJX_SYP9oyMzUAtg" videos left brasil first) ;; João Carvalho
		("https://youtube.com/feeds/videos.xml?channel_id=UCYM7I0m-I9EVB-5gaBqiqbg" videos left economics second) ;; Michael Roberts economist
		("https://youtube.com/feeds/videos.xml?channel_id=UCVBfIU1_zO-P_R9keEGdDHQ" videos left economics first) ;; Paul Cocksott
		("https://youtube.com/feeds/videos.xml?channel_id=UCH83Eu71r06OtkXRDfnh1Eg" videos left economics sociology second) ;; Rhizomes
		;; Music
		("https://youtube.com/feeds/videos.xml?channel_id=UCnkp4xDOwqqJD7sSM3xdUiQ" videos music second) ;; Adam Neely
		("https://youtube.com/feeds/videos.xml?channel_id=UCRYhCg0DHloE9gn-PAiAYNg" videos music second) ;; deep cuts
		("https://youtube.com/feeds/videos.xml?channel_id=UCgtg2afk_O_YE6zL1meXtJQ" videos music second) ;; theonewhoCheekiBreeki
		;; shitpost
		("https://youtube.com/feeds/videos.xml?channel_id=UCHnK83dqyPvFo6GX0rCwn8A" videos shitpost second) ;; apetor
		("https://youtube.com/feeds/videos.xml?channel_id=UCokojG4PTMGPHZFZHgi4cgQ" videos shitpost imageboard second) ;; Imageboard Archive
		;; Tech
		("https://videos.lukesmith.xyz/feeds/videos.xml?videoChannelId=2" videos tech peertube first) ;; Luke Smith
		("https://youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" videos tech second) ;; Mental Outlaw
		("https://youtube.com/feeds/videos.xml?channel_id=UCxkMDXQ5qzYOgXPRnOBrp1w" videos tech emacs second) ;; Mike Zamansky
		("https://youtube.com/feeds/videos.xml?channel_id=UCgREucssIfY9e0Iy3yhse8w" videos tech second) ;; nixcasts
		("https://youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" videos tech emacs second) ;; Protesilaos Stavrou
		("https://youtube.com/feeds/videos.xml?channel_id=UC8ENHE5xdFSwx71u3fDH5Xw" videos tech second) ;; ThePrimeagen
		("https://youtube.com/feeds/videos.xml?channel_id=UCsnGwSIHyoYN0kiINAGUKxg" videos tech second) ;; Wolfgang's Channel
		("https://youtube.com/feeds/videos.xml?channel_id=UCvrQyT2xhsxq2a4Xopf2JTA" videos tech emacs second) ;; Zaeph
		;; Anime
		("https://youtube.com/feeds/videos.xml?channel_id=UCGgy9QqFwElrVg4vf6QNX_A" videos anime vidya second) ;; BestGuyEver
		("https://youtube.com/feeds/videos.xml?channel_id=UCaA5wWERI1gyl-iwTkb0JTw" videos anime music second) ;; Hobbes Sakuga
		("https://youtube.com/feeds/videos.xml?channel_id=UCySYksw8XF41eogHd0qwZbQ" videos anime music second) ;; Ogeid
		("https://youtube.com/feeds/videos.xml?channel_id=UCcDfv0bFBkxJsTIr7VejNRA" videos anime music imageboard second) ;; Sync Fag
		("https://youtube.com/feeds/videos.xml?channel_id=UCWUqXkwu7VqsmtH1SrHdkeA" videos anime podcast second) ;; The Po D. Cast
		;; Film
		("https://youtube.com/feeds/videos.xml?channel_id=UCWTFGPpNQ0Ms6afXhaWDiRw" videos film second) ;; Now You See It
		("https://youtube.com/feeds/videos.xml?channel_id=UCSc16oMxxlcJSb9SXkjwMjA" videos film second) ;; YourMovieSucksDOTorg
		;; Vidya
		("https://youtube.com/feeds/videos.xml?channel_id=UCmBXkjKD8w6bbjUzNjcDtQA" videos vidya second) ;; illusory wall
		("https://youtube.com/feeds/videos.xml?channel_id=UC477Kvszl9JivqOxN1dFgPQ" videos vidya second) ;; Iron Pineapple
		("https://youtube.com/feeds/videos.xml?channel_id=UCyhnYIvIKK_--PiJXCMKxQQ" videos vidya second) ;; Joseph Anderson
		("https://youtube.com/feeds/videos.xml?channel_id=UCb_sF2m3-2azOqeNEdMwQPw" videos vidya first) ;; Matthewmatosis
		("https://youtube.com/feeds/videos.xml?channel_id=UCf2RHSSbKcE7NzUpuf7sWNA" videos vidya shitpost second) ;; moike kobe
		("https://youtube.com/feeds/videos.xml?channel_id=UC1JTQBa5QxZCpXrFSkMxmPw" videos vidya second) ;; Raycevick
		("https://youtube.com/feeds/videos.xml?channel_id=UCD6VugMZKRhSyzWEWA9W2fg" videos vidya shitpost second) ;; SsethTzeentach
		("https://youtube.com/feeds/videos.xml?channel_id=UCsvn_Po0SmunchJYOWpOxMg" videos vidya first) ;; videogamedunkey
		("https://youtube.com/feeds/videos.xml?channel_id=UC17vsYVoIwch5UzPar1LDmQ" videos vidya second) ;; ymfah
		("https://youtube.com/feeds/videos.xml?channel_id=UCPPzle1FH3iYlFjwGrF9O3g" videos vidya shitpost second) ;; ymfahVEVO
		;; Other
		("https://youtube.com/feeds/videos.xml?channel_id=UCZGkMJEmCR1IqUduqt1uFUw" videos other second) ;; noriyaro
		("https://youtube.com/feeds/videos.xml?channel_id=UCBa659QWEk1AI4Tg--mrJ2A" videos other second) ;; Tom Scott
		))
