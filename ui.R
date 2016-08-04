library(shiny)
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Přijímací zkoušky na FSS MU: Srovnání OSP a TSP"),
  
  sidebarPanel(
    checkboxInput('showPanel2', 'Zobraz pokročilé nastavení', F),
    checkboxInput('showPanel3', 'Zobraz nastavení simulace', F),
    h3("Základní nastavení (uchazeč)"),
    h4("Parametry přijímacího řízení"),
    numericInput("nOSP", "Počet opakování testu OSP:", 5, min=1, width="300px"),
    numericInput("nZSV", "Počet opakování testu ZSV:", 5, min=1, width="300px"),
    sliderInput("cut", "Součtový percentil nutný pro přijetí:", 163, min=0, max=200),
    tags$p(
      HTML("Zadej skóre nutné pro přijetí, které najdeš např. <a href='http://www.fss.muni.cz/admission/admission_process' target='_blank'>zde</a>.")
    ),
    br(),
    conditionalPanel(condition = 'input.showPanel2',
      h3("Pokročilé nastavení (FSS)"),
      h4("Reliability testů"),
      sliderInput("rOSP", "reliabilita testu OSP:", .8065, min=0, max=1),
      sliderInput("rTSP", "reliabilita testu TSP:", .8, min=0, max=1),
      sliderInput("rZSV", "reliabilita testu ZSV:", .8, min=0, max=1),
      br(),
      h4("Vztah testů"),
      numericInput("OSPTSP", "korelace OSP a TSP:", .7, 0, 1, .01, "300px"),
      numericInput("OSPZSV", "korelace OSP a ZSV:", .6, 0, 1, .01, "300px"),
      numericInput("ZSVTSP", "korelace TSP a ZSV:", .6, 0, 1, .01, "300px"),
      sliderInput("cohenD", "Rozdíl v obtížnosti TSP-ZSV (Cohenovo d)", 0, min=-1.5, max=1.5),
      sliderInput("vahy1", "váha studijních předpokladů (TSP/OSP); váha ZSV je 1-zvolená hodnota", .4, min=0, max=1),
      br()
    ),
    conditionalPanel(condition = 'input.showPanel3',
      h3("Nastavení simulace"),
      numericInput("N", "počet respondentů (vyšší počet je přesnější, ale výpočetně náročnější):", 10000, max=10000, width="300px"),
      selectInput("prepinac1", "Použít u kumulativního grafu pravý percentil namísto součtového skóre?", 
                  list("součtové skóre"=1,"percentil ZSV+OSP"=2,"percentil ZSV+TSP"=3)),
      numericInput("seed", "set-seed:", NA, width="300px")
    )

  ),
  
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    
    tabsetPanel(
      tabPanel("Úvod", 
    
    br(),
    checkboxInput('showPanel1', 'Zobraz podrobnou nápovědu k nastavení aplikace', F, width="700px"),
    conditionalPanel(condition = 'input.showPanel1',
                     wellPanel(
                       p("Pro zobrazení pokročilého nastavení a nastavení simulace je nutné zakliknout příslušnou volbu 
                         v ovládacím panelu."),
                       h4("Základní nastavení (pro uchazeče)"),
                       p("Tyto parametry simulují to, nakolik ovlivní výsledek přijímacího řízení uchazeč (výběrem 
                         OSP/TSP a počtem pokusů)."),
                       tags$ul(
                         tags$li("Počet opakování: Kolikrát plánujete absolvovat OSP (resp. ZSV)."),
                         tags$li("Součtový percentil nutný pro přijetí: Nastavte součtový percentil podle zvoleného oboru")
                         ),
                       h4("Pokročilé nastavení"),
                       p("Následující parametry simulují jednak ty aspekty přijímacího řízení, které ovlivňuje 
                         Fakulta sociálních studií nastavením přijímacího řízení, a jednak ty, které závisí na kvalitě 
                         používaných testů. Předem jsou nastavené takové hodnoty, o kterých buď víme nebo si alespoň myslíme, 
                         že odpovídají parametrům přijímacího řízení ve skutečnosti."),
                       tags$ul(
                         tags$li("Reliability testů: Zadejte reliability jednotlivých testů. Jde o reliabilitu ve smyslu 
                                 klasické testové teorie; pro účely aplikace je nejvýhodnější test-retest reliabilita 
                                 (použít však lze například i vnitřní konzistence testu). Lze nicméně samozřejmě 
                                 použít i test-retest reliabilitu IRT-based skórů, u kvalitně vytvořeného testu by heteroskedascita 
                                 chyby měření neměla u drtivé většiny uchazečů zásadním způsobem ovlivnit interpretovatelnost 
                                 výsledků."),
                         tags$li("Vztah testů: Korelace pozorovaných hrubých (příp. standardizovaných) skórů. Nejde 
                                 o korelace percentilů."),
                         tags$li("Rozdíl v obtížnosti TSP-OSP: Pokud se liší výběrová populace absolventů zkoušek TSP a OSP odlišná, 
                                 lze tímto způsobem simulovat rozdíl. Zadejte velikost rozdílu v jednotkách velikosti efektu Cohenova d, 
                                 tedy rozdíl průměrů pozorovaných skórů uchazečů o studium v násobcích směrodatné odchylky. 
                                 Hodnoty větší než 0 znamenají, že stejně šikovný 
                                 uchazeč získává v TSP vyšší výsledek než v OSP (a naopak), a bude tedy zvýhodněn oproti uchazečů, kteří si zvolili OSP. 
                                 Předem je nastavená hodnota 0, která 
                                 značí žádný rozdíl."),
                         tags$li("Váha studijních předpokladů: Procentuální podíl, jaký ve výsledku představuje OSP nebo TSP. 
                                 Váha ZSV je automaticky vypočítána jako zbytek do jedné.")
                         ),
                       h4("Nastavení simulace"),
                       p("Následující nastavení ovlivňuje chování simulace. Nejde o parametry, které by figurovaly ve skutečném 
                         přijímacím řízení."),
                       tags$ul(
                         tags$li("Počet respondentů: Počet simulovaných respondentů (pro každého jsou simulovány veškeré skóry). 
                                 Menší počet je výpočetně méně náročný, vyšší číslo vede k větší přesnosti simulace. Doporučujeme 
                                 nenastavovat méně než 500 a více než 50000."),
                         tags$li("Typ použitého skóre: nemá valný význam, ovlivňuje podobu 
                                 (a pokročilejší interpretaci grafů), konkrétně ovlivňuje škálu na ose x. 
                                 Jako výchozí hodnota je nastaveno součtové skóre (tedy pozorovaný součet) percentilů, lze 
                                 ale zvolit i tzv. pravý percentil, tedy součet percentilů bez jakékoliv chyby měření 
                                 (a to zvlášť pro variantu TSP nebo OSP)."),
                         tags$li("set-seed: Výchozí hodnota pro generátor pseudonáhodných čísel.")
                         
                       )
                     )
    ),
    tags$div(
      HTML("
<p>Přijímací zkoušky na <a href='http://fss.muni.cz' target='_blank'>Fakultu sociálních studií Masarykovy univerzity</a> 
mají od roku 2016 novou podobu &ndash; uchazeč má na výběr, zda absolvuje test studijních předpokladů připravený MU (TSP), nebo Obecné studijní
předpoklady od společnosti SCIO (OSP). V každém případě má tento test váhu 40 % na výsledný počet bodů, zbylých 60 % má test Základů společenských
věd (ZSV) taktéž od SCIO.<sup>1</sup></p>
<p>Kvalitní přepočet počtu bodů (percentilů) mezi OSP a TSP bez podrobných dat na úrovni jednotlivých zodpovězených položek není možný. 
Oba testy neměří &bdquo;stejné studijní předpoklady&ldquo;, ale poněkud jiné. Zároveň se pak percentil počítá z&nbsp;jiné &bdquo;populace&ldquo; &ndash; 
nelze předpokládat, že uchazeči absolvující SCIO testy a testy TSP jsou &bdquo;stejně dobří&ldquo;.</p>

<p>Situaci nicméně komplikuje jedna zásadnější věc, které je věnována tato aplikace. Testy od SCIO, tedy OSP a ZSV, je možné absolvovat vícekrát, 
přičemž se započítává nejvyšší počet bodů. Protože každý test měří s určitou chybou, opakované testování zvyšuje pravděpodobnost, 
že tato náhodná chyba &bdquo;zahraje ve prospěch&ldquo; uchazeče. I po odhlédnutí od toho, že při opakovaném absolvování testu 
pochopitelně není &bdquo;pravý výkon&ldquo; uchazeče stejný (člověk se učí, připravuje atp.), uchazeči, kteří test absolvují vícekrát,
jsou na základě pravděpodobnosti zvýhodněni oproti uchazečům, kteří test absolvují jen jednou.</p>
<p>Můžeme to připodobnit házení kostkou, kdy k přijetí je nutné, aby padla šestka. Modrá kostka je OSP, zelená TSP. Uchazeč, který si vybere modrou kostku a 
hodí pětkrát, má pochopitelně výrazně vyšší pravděpodobnost k přijetí než uchazeč s kostkou zelenou. Používané testy samozřejmě mají 
náhodnou složku výsledku menší než 100%, výsledky jsou však ovlivněny obdobným způsobem.</p>
<p><strong>Tato aplikace tedy slouží ke srovnání pravděpodobnosti přijetí při výběru OSP nebo TSP
a zvoleném počtu pokusů pro každý test.</strong> Protože toto vyčíslení exaktním výpočtem je poměrně komplikované, 
výsledky simulujeme prostřednictvím běžných statistických postupů. Ty mají určité předpoklady<sup>2</sup>, které by však 
platily i pro exaktní výpočet.</p>
<h4>Hlavní závěry v bodech:</h4>
<ul style='font-weight: 700;'>
<li>Větší šanci na přijetí mají uchazeči, kteří zvolí test OSP...</li>
<li>... a test OSP i ZSV absolvují tolikrát, kolikrát to jen lze.</li>
</ul>
<p>&copy; 2016 Hynek Cígler a kol.<br />Katedra psychologie, Fakulta sociálních studií MU</p>
<p>Zdrojový kód je k dispozici na <a href='https://github.com/hynekcigler/prijimackyFSS'>https://github.com/hynekcigler/prijimackyFSS</a>. 
<br />
<hr />
<p><small><sup>1</sup> Zcela v pořádku není ani samotný součet percentilů &ndash; percentilová stupnice není &bdquo;lineární&ldquo;, rozdíl schopností nutných k posunu 
z&nbsp;např. 50. percentilu na 55. je výrazně menší, než z&nbsp;90. na 95. percentil (konkrétně 2,9krát). Součet percentilů proto poněkud zvýhodňuje uchazeče,
kteří mají &bdquo;vyrovnaný&ldquo; profil schopností oproti těm, kteří jsou v&nbsp;jedné oblasti (ZSV nebo studijní předpoklady) lepší než ve druhé. 
To však může být záměr a tento jev nijak výrazně neomezuje možnost srovnání OSP a TSP. I tak se tomuto jevu stručně věnujeme na záložce 
Součet percentilů.</small></p>
<p><small><sup>2</sup> <i>Prvním předpokladem</i> výpočtů je to, že populace osob, která absolvuje TSP a OSP, je stejně &bdquo;šikovná&ldquo;. To samozřejmě zcela neplatí, a je 
to jedním z omezení intepretace prezentovaných výsledků. Na druhou stranu, neshodnosti obou populací ve skutečnosti komplikují přímo samotné srovnání 
výsledků, a to dost možná výraznějším způsobem. <i>Druhým předpokladem</i> (jen pro některé z výpočtů) je to, 
že uchazeči o studium jsou náhodným výběrem z populace, z níž 
je počítán percentil. Tento předpoklad pochopitelně není zcela splněný. <i>Další předpoklady</i> jsou již statistického rázu 
(zejm. normální rozložení skórů a homoskedascita chyby měření), které by však neměly příliš ovlivňovat 
výsledky simulace.</small></p>
     ")
    )
    
      ),
    tabPanel("Pravděpodobnost přijetí",
    
    
    h2("Pravděpodobnost přijetí"),
    h4("Podle úrovně schopnosti uchazeče"),
    p("Graf ukazuje, nakolik se zvýší vaše pravděpodobnost přijetí při zvoleném počtu opakování (přerušovaná čára), 
      pokud absolvujete test OSP (červená) nebo (TSP) zelená, oproti jedinému pokusu (plná čára)."),
    plotOutput("pprijeti", width= "700px"),
    h4("Pro náhodného uchazeče"),
    p("Pravděpodobnost přijetí, respektive počet přijatých, podle toho, zda zvolí OSP nebo TSP, a podle počtu pokusů u SCIO testů. 
      Lidé, kteří testy absolvují vícekrát, mají násobně vyšší šanci na přijetí, než lidé, kteří testy absolvují jednou."),
    h5("Pokud by měření bylo bez chyby (reliability rovné 1)"),
    tableOutput("text1"),
    h5("Při zvolené chybě měření (podle reliability)"),
    tableOutput("text2"),
    br()
    
    ),
    tabPanel("Velikost zvýhodnění",
             
             h2("Velikost zvýhodnění při různých kombinacích testů a počtu pokusů"),
             plotOutput("scatter1", inline=T),
             plotOutput("scatter2", inline=T),
             br(),
             br(),
             plotOutput("scatter3", inline=T),
             plotOutput("scatter4", inline=T),
             br(),
             br(),
             plotOutput("scatter5", inline=T),
             br(),
             
             h2("Srovnání závěru napříč různými kombinacemi testů a pokusů"),
             p("V pravém dolním rohu je velikost zvýhodnění vyjádřená jako poměr šancí; tedy kolikrát je vyšší šance na přijetí ve výhodnější 
               kombinaci pokusů a počtu termínů oproti méně výhodné variantě."),
             splitLayout(
               h4("Srovnání osob, kteří si vyberou OSP nebo TSP ", br(), "a všechny testy absolvují jedenkrát"),
               h4("Zvýhodnění lidí, využívajících všech možností ", br(), "SCIO oproti těm, kteří absolvují TSP a ZSV ", br(), "zkusí, kolikrát to jen jde")
               
             ),
             
             splitLayout(
               tableOutput("srovnaniOSP1TSP1"),
               tableOutput("srovnaniOSPnTSPn")
               
             ),
             splitLayout(
               h4("Zvýhodnění lidí, využívajících všech možností ", br(), "SCIO oproti těm, kteří absolvují oba ", br(), "SCIO testy jen jednou"),
               h4("Zvýhodnění lidí, kteří si vybrali TSP a využili ", br(), "všechni možnosti na ZSV \noproti těm, ", br(), "kteří na ZSV šli jen jednou")
               ),
             splitLayout(
               tableOutput("srovnaniOSPnOSP1"),
               tableOutput("srovnaniTSPnTSP1")
               ),
             h4("Zvýhodnění lidí, využívajících všech možností SCIO ", br(), "oproti těm, kteří absolvují TSP a ", br(), "neuvědomí si, že ZSV mohou vícekrát"),
             tableOutput("srovnaniTSP1OSPn")
             
    ),
    tabPanel("Správnost přijetí",

    
    h2("Správnost přijetí"),

    splitLayout(
      h4("1x ZSV + 1x OSP"),
      h4("Nx ZSV + Nx OSP")
    ),
    splitLayout(
      tableOutput("spravnostOSP1"),
      tableOutput("spravnostOSPn")
    ),
    splitLayout(
      plotOutput("hist1", width= "400px"),
      plotOutput("hist2", width= "400px")
    ),
    
    splitLayout(
      h4("1x ZSV + 1x TSP"),
      h4("Nx ZSV + 1x TSP")
    ),
    splitLayout(
      tableOutput("spravnostTSP1"),
      tableOutput("spravnostTSPn")
    ),
    splitLayout(
      plotOutput("hist3", width= "400px"),
      plotOutput("hist4", width= "400px")
    ),
    
    h2("Reliabilita celkového výsledku"),
    p("Následuje odhad reliability celkového skóru podle toho, kterou z kombinací testů a počtu pokusů si uchazeč zvolí. 
      Odhad reliability je odvozen z druhé mocniny korelace pravých a pozorovaných percentilů, resp. hrubých skórů. 
      Korelace percentilů je podhodnocena z důvodu nelinearity dat; nicméně s percentily se už tak počítá jako s intervalovými skóry tím,
      že jsou sčítány."),
    tableOutput("reliabilita"),
    plotOutput("scatter6", inline=T),
    plotOutput("scatter7", inline=T),
    br(),
    plotOutput("scatter8", inline=T),
    plotOutput("scatter9", inline=T),
    br(),
    br()
    
    ),

    tabPanel("Součet percentilů",
             h2("Srovnání pozorovaného součtového percentilu a pravého skóre"),
             p("Tato záložka slouží pouze k informaci, jak moc zkresluje výsledky fakt, že jsou sčítány percentily namísto hrubých skórů. 
               Je vidět, že ve všech případech jsou mírně znevýhodněni ti nejlepší a naopak zvýhodněni ti nejhorší uchazeči. 
               Tento jev je příčinou nižších reliabilit součtových percentilů oproti hrubému skóre, prezentovaném na záložce Správnost přijetí."),
             plotOutput("srovnaniTSP1", width= "700px"),
             plotOutput("srovnaniOSP1", width= "700px"),
             plotOutput("srovnaniTSPn", width= "700px"),
             plotOutput("srovnaniOSPn", width= "700px"),
             plotOutput("srovnanitrueTSP", width= "700px"),
             plotOutput("srovnanitrueOSP", width= "700px")
             
             ),
    
    
    
    
    
    
    tabPanel("Nápověda",
             h2("Stručná nápověda k interpretaci výsledků"),
             h3("Pravděpodobnost přijetí"),
             p("V této sekci naleznete, jaká je pravděpodobnost přijetí pro toho stejného uchazeče při výběru různých variant testu (při určité úrovni jeho schopnosti), resp. pro náhodného uchazeče (z čehož lze zjistit celkový počet přijatých za předpokladu, že všichni uchazeči zvolí stejnou variantu).  "),
             h4("Podle úrovně uchazeče"),
             p("Vliv opakovaného testování na dosažený součtový percentil v přijímacím řízení se liší podle toho, jak dobrý je uchazeč. Největší zvýhodnění získají opakovanými pokusy ti respondenti, kteří jsou ve skutečnosti „těsně pod čarou“ (kteří by při jediném pokusu měli o něco menší než 50% šanci na přijetí). Slouží ke srovnání pravděpodobnosti přijetí při volbě různých variant testů (OSP vs. TSP, počet pokusů). Na ose y je pravděpodobnost, že bude ten stejný uchazeč přijat při volbě různé varianty testu. Osa x nelze adekvátně interpretovat (záleží na volbě v pokročilém nastavení simulace), všechny křivky jsou však vynesené stejným způsobem. Body křivek „nad sebou“ tak odpovídají stejným uchazečům. "),
             h4("Pro náhodného uchazeče"),
             p("První tabulka ukazuje procento přijatých při různých variantách za předpokladu nulové chyby měření. Protože v takovém případě nehraje roli počet opakování (uchazeč při opakování dosáhne stejného skóre, protože test neměří s chybou), jsou k dispozici pouze varianty OSP+ZSV a TSP+ZSV. První dvě pole ukazují pozorovaný podíl přijatý při zvolené bodové hranici nutné pro přijetí. Předpoklad je spočítán následujícím způsobem: Pokud je např. hranice 160 součtových percentilů ze dvou testů, mohli bychom předpokládat, že podíl přijatých bude 160/2 = 80 % osob. Pravděpodobnost přijetí je tedy 100-80 = 20 % (0,20). Jak vidíme, tento předpoklad neplatí, a to právě z důvodu, že jsou sčítány percentily (viz pozn. na titulní straně)."),
             p("Druhá tabulka ukazuje podíl přijatých (tedy pravděpodobnost přijetí náhodného uchazeče bez ohledu na jeho schopnosti) při nastavené chybě měření v Pokročilém nastavení, a to pro různé kombinace testů a různý počet opakování nastavený v Základním nastavení. První řádek udává pravděpodobnost přijetí, druhý řádek počet přijatých při počtu simulovaných respondentů zvolených v Pokročilém nastavení. TIP: Nastavte počet respondentů např. na desetinásobek počtu uchazečů. Na druhém řádku pak naleznete desetinásobek počtu přijatých uchazečů za předpokladu, že všichni zvolí stejnou kombinaci testů a počtu opakování."),
             h3("Velikost zvýhodnění"),
             p("Na této záložce si můžete zjistit, kolik bodů (součtových percentilů) získáte či ztratíte odlišnou volbou testů a počtu opakování, případně rozdíl v počtu přijatých při různých volbách jednotlivých variant."),
             h4("Grafy"),
             p("Grafy umožňují přímo srovnat bodový rozdíl při různých volbách. Černé body odpovídají jednotlivým simulovaným uchazečům. Červená přímka předkládá očekávaný vztah obou variant, zelená křivka pak skutečný vztah obou variant. Červená přímka neodpovídá plně ose prvního a třetího kvadrantu z důvodu chyby měření (přímka popisující vztah dvou testů je vždy poněkud plošší, a to tím více, čím větší je chyba měření). V legendě každého grafu je dále k dispozici bodové zvýhodnění uchazečů vyjádřené pomocí mediánu rozdílů jednotlivých uchazečů („Md = ??“). Medián rozděluje soubor hodnot na dvě stejně velké poloviny – polovina lidí je tedy zvýhodněna více, než je hodnota mediánu, a polovina naopak méně (medián je vhodnější než průměr, protože je méně ovlivněn extrémními hodnotami)."),
             p("Osy grafů jsou vždy označeny typem testu a počtem opakování (např. 1x TSP a Nx ZSV znamená, že uchazeč absolvoval jeden pokus u TSP testu a všechny pokusy u ZSV testu)."),
             p("TIP: Po prvním pokusu najděte svůj součtový percentil na ose x a dohledejte příslušnou hodnotu červené (OSP) resp. zelené (TSP) nepřerušované čáry na ose y. Následně na ose y vyhledejte hodnotu pro čáru stejné barvy, ale přerušovanou – rozdíl udává počet bodů, které nejpravděpodobněji získáte, absolvujete-li všechny přijímací zkoušky v počtu pokusů zvoleném v Základním nastavení."),
             h4("Tabulky"),
             p("Tabulky zobrazují stejné statistiky, jako grafy výše, a to ve stejném pořadí. Rozdíl je v tom, že tabulky obsahují souhrnné počty pro všechny simulované uchazeče (jejichž počet lze volit v oddílu Nastavení simulace). Tabulky ukazují procentuální součty řádku, tedy celkový podíl přijatých a nepřijatých pro tu kterou variantu."),
             p("V pravém dolním rohu každé tabulky je tzv. „podíl šancí“, který udává, kolikrát vyšší šanci na přijetí má průměrný respondent v případě, že zvolí výhodnější kombinaci vybraných testů a počtu opakování."),
             h2("Správnost přijetí"),
             p("Tato záložka předkládá informace o správnosti přijetí. Pro každou ze čtyř zvažovaných kombinací typu zvoleného testu (OSP vs. TSP) a počtu pokusů u SCIO testů uvádí počty simulovaných respondentů, kteří by byli přijati, pokud by testy měřily zcela přesně, a počty skutečně přijatých respondentů. Dále pro každou z těchto čtyř variant předkládá rozložení pravých součtových percentilů uchazečů, kteří byli přijatí, a kteří nebyli. "),
             p("Ukazatelem „měl být přijat“ je fakt, zda pravý percentil překročil nastavenou hodnotu pro přijetí."),
             p("Ve druhé části stránky jsou k dispozici reliability celkového výsledku pro ty stejné kombinace testů a počty pokusů. Reliabilita nabývá hodnot od 0 do 1 a čím je vyšší, tím je menší chyba měření. K dispozici jsou i grafy srovnávající skutečné a pozorované percentily. Na nich červená přímka udává předpokládaný regresní vztah na základě reliability, zelená křivka vztah skutečný.")
             
      
    )
    
    )
  )
))