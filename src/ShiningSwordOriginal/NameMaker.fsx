// This script is for creating more name tables easily from https://tekeli.li/onomastikon/Celtic/Ireland/Celtic-Male.html or similar

open System.Text.RegularExpressions
// There are so many names we break them out into a separate file

let hmale = """Aanand	Abadhya	Abala
Abalendu	Abayankara	Abdhisayana
Abhasa	Abhasvara	Abhata
Abhay	Abhaya	Abhayananda
Abhayasinha	Abhayda	Abhayprada
Abhi	Abhibhu	Abhicandra
Abhidi	Abhidipa	Abhidyu
Abhigita	Abhihava	Abhihita
Abhijana	Abhijata	Abhijaya
Abhijeet	Abhiji	Abhijit
Abhijnana	Abhijvala	Abhik
Abhika	Abhikama	Abhikanksa
Abhikhyana	Abhilasha	Abhilasin
Abhimand	Abhimani	Abhimanyusuta
Abhimatjit	Abhimoda	Abhinabhas
Abhinamin	Abhinanda	Abhinandana
Abhinandin	Abhinandita	Abhinatha
Abhinav	Abhinava	Abhinavan
Abhinita	Abhinivesa	Abhipada
Abhipala	Abhipuspa	Abhira
Abhiraja	Abhiraksa	Abhiraksita
Abhirama	Abhiramana	Abhirastra
Abhiratra	Abhiru	Abhiruca
Abhiruci	Abhirucira	Abhirupa
Abhisala	Abhisara	Abhiseka
Abhishek	Abhisikta	Abhisneha
Abhisoka	Abhisri	Abhisu
Abhisumat	Abhita	Abhivada
Abhivira	Abhiviraja	Abhiyanta
Abhiyutaksika	Abhra	Abhrakasin
Abhramu	Abhramupriya	Abhranaga
Abhranta	Abhraroha	Abhrottha
Abhyagni	Abhyavarsini	Abhyudaya
Abhyudita	Abir	Abjit
Acalapati	Acalendra	Acananas
Acanda	Acarya	Acchoda
Acchundra	Achyuta	Acyutaraya
Acyutayu	Adambara	Adarsa
Adarsh	Adarshpal	Addana
Adelika	Adesa	Adesh
Adharma	Adhasasiras	Adhibu
Adhiksita	Adhilokanatha	Adhinatha
Adhipa	Adhira	Adhiraja
Adhiratha	Adhirohna	Adhirukma
Adhisa	Adhisvara	Adhita
Adhivahana	Adhiviraja	Adhrgu
Adhrsya	Adhvara	Adhvaryu
Adhyapayana	Adi	Adidaitya
Adilaksmana	Adima	Adimula
Adina	Adinatha	Adipa
Adiraja	Adiratha	Adisa
Adisesan	Adisimah	Adisisira
Adisvara	Adit	Adita
Aditeya	Aditi	Adityabhakta
Adityagarbha	Adityaketu	Adityaprabha
Adityasena	Adityavardhana	Adityavarman
Adityavarna	Adityesa	Admani
Adra	Adri	Adrigu
Adrisa	Adrupa	Adura
Adusita	Advaya	Advayananda
Advika	Advitiya	Adyasarana
Adyota	Agadhi	Agama
Agarva	Agasti	Agavaha
Agendra	Agha	Aghamarsana
Agharika	Agnibahu	Agnibha
Agnibhuti	Agnibija	Agnidatta
Agnidhra	Agnidhraka	Agnika
Agnikana	Agniketu	Agnima
Agnimasa	Agnimitra	Agnimukha
Agnipa	Agnira	Agnirajan
Agniruha	Agnisambhava	Agnisika
Agnisinha	Agnisoma	Agnisri
Agnistu	Agnistuta	Agnitejas
Agnivallabha	Agnivamin	Agnivarna
Agnivesa	Agnivesya	Agnivirya
Agnpurna	Agraha	Agraja
Agrasena	Agrayana	Agrayani
Agreni	Agrima	Aha
Ahamyati	Ahan	Ahankara
Ahanmani	Ahannatha	Ahanti
Ahar	Ahara	Aharbandhava
Ahavaniya	Ahavaniym	Ahika
Ahilocana	Ahima	Ahinagu
Ahiratha	Ahirbudhnya	Ahlad
Ahobala	Ahovira	Ahuka
Ahupathi	Ahuta	Ahuti
Ahvana	Aila	Aindradyumni
Aitareya	Ajagara	Ajakara
Ajakasva	Ajakava	Ajalika
Ajamila	Ajamukha	Ajandeva
Ajanidha	Ajapa	Ajapala
Ajaparsva	Ajasra	Ajata
Ajavindu	Ajay	Ajigarta
Ajina	Ajinka	Ajira
Ajit	Ajitabha	Ajitanatha
Ajitapala	Ajitatman	Ajodara
Ajyapa	Akalanka	Akalmasa
Akalpa	Akalusa	Akama
Akampana	Akampita	Akanistha
Akantaka	Akapivata	Akara
Akarkkara	Akarsana	Akarsita
Akasa	Akasacamasa	Akasagarbhi
Akatha	Akhanda	Akhil
Akhila	Akhilatman	Akhilendra
Akhilesa	Akhilesavara	Akila
Akopa	Akra	Akrida
Akrodhana	Akrosa	Akrsasasva
Akrsnakarman	Akrtavrana	Akrti
Akrura	Aksaja	Aksaka
Aksakumara	Aksamata	Aksapada
Aksata	Aksaya	Akshay
Akshey	Aksina	Aksita
Aksobhya	Aksunna	Aktu
Akuntha	Akupara	Akupya
Akurca	Alagesa	Alagesan
Alaghu	Alakaprabha	Alesa
Alia	Alin	Alinda
Alipriya	Alobhin	Alok
Aloka	Alola	Alolupa
Alopa	Alpasaras	Alugu
Amad	Amahatha	Amahiyu
Amaka	Amalamani	Amalatman
Amalgarbha	Amama	Amana
Amanath	Amanda	Amandeep
Amandip	Amandipa	Amanjit
Amanjot	Amanpreet	Amanthu
Amanusa	Amar	Amara
Amaracandra	Amaradatta	Amarajita
Amarajota	Amarajyoti	Amaranjaya
Amaraprita	Amararatna	Amarasinha
Amardit	Amardita	Amaresh
Amarika	Amarisa	Amarishnu
Amarjit	Amarnath	Amaropama
Amarpreet	Amarsa	Amartya
Amaru	Amaruka	Amarupam
Amaruttama	Amas	Amava
Amavasu	Amaya	Ambaka
Ambara	Ambastha	Ambasthya
Ambava	Ambhassara	Ambhoja
Ambhoruha	Ambhrna	Ambu
Ambuda	Ambudhara	Ambumani
Ambunidhi	Ambupati	Amburuha
Ambuvica	Amini	Amir
Amirtharaj	Amisa	Amish
Amit	Amita	Amitabh
Amitabha	Amitadjuti	Amitagati
Amitakratu	Amitan	Amitaruci
Amitatman	Amitaujas	Amitav
Amitava	Amitaya	Amitayus
Amitdhvaja	Amitkumar	Amitojas
Amitosa	Amitrajit	Amlana
Amod	Amoda	Amodin
Amoghadarsana	Amoghadarsin	Amoghasiddhi
Amoghavajra	Amoghavarsa	Amoha
Amol	Amola	Amolak
Amolaka	Amprithu	Amrapali
Amreet	Amrish	Amrit
Amrita	Amritadana	Amritaka
Amritaksara	Amritam	Amritamaya
Amritananda	Amritaprabha	Amritejas
Amtesvara	Amul	Amulya
Amura	Amurtarajas	Amurtarayas
Ana	Anabhisastra	Anadhika
Anadhrishya	Anadinava	Anadrishti
Anaghadru	Anakranta	Analajit
Anama	Anamitra	Anamiva
Anamol	Anana	Anand
Ananda	Anandaghana	Anandagiri
Anandam	Anandamrta	Anandaprabha
Anandaprada	Anandasagara	Anandatman
Anandavardhana	Anandbhuj	Anandi
Anandin	Anandita	Anangada
Anangam	Anangapala	Anangapida
Anant	Ananta	Anantadeva
Anantaguna	Anantajit	Anantaram
Anantarama	Anantavirya	Ananth
Ananthakrishanan	Ananthakrishnan	Anantharaman
Anantya	Anapana	Anapayacola
Anargha	Anari	Anarti
Anarvva	Anasaya	Anasin
Anasuri	Anasvan	Anasvara
Anathapinda	Anava	Anavaratha
Anavatapta	Ancala	Andaka
Andha	Andhraka	Androsa
Anenas	Anenasya	Anga
Angabhu	Angama	Anganemi
Angaraka	Angarasetu	Angaristha
Angasvami	Angavaha	Anghari
Angirasa	Angosin	Angsu
Angusa	Anhu	Aniha
Anika	Anikavidarana	Aniketa
Anil	Anila	Anilabha
Anilkumar	Animan	Animesa
Aninadata	Aninata	Anirban
Anirbana	Anirjita	Aniruddh
Aniruddha	Anirudh	Anirudha
Anirudhh	Anita	Anitabha
Aniteja	Anitha	Anjaka
Anjali	Anjami	Anjan
Anjana	Anjanaparvan	Anjasa
Anjika	Anjuli	Ankolita
Ankur	Ankura	Annadeva
Annamaya	Anniruddha	Anogopta
Anoop	Anouar	Anrita
Ansa	Ansaka	Ansala
Ansansa	Ansarin	Anshul
Anshuman	Ansin	Ansrutas
Ansu	Ansujala	Ansuka
Antama	Antara	Antardhana
Antardhi	Antariksa	Antarjyotis
Antarvedi	Anti	Antideva
Antya	Anu	Anubhaj
Anubhava	Anubhavya	Anubodha
Anucakra	Anucana	Anudara
Anudatta	Anudesya	Anudruhu
Anudrutta	Anudvega	Anugayas
Anugra	Anugya	Anuha
Anuhlada	Anuhrada	Anuhu
Anuj	Anuja	Anujay
Anujestha	Anuka	Anukarsa
Anukasa	Anukula	Anumita
Anumodana	Anuna	Anunavarcas
Anup	Anupa	Anupam
Anupama	Anupati	Anuprabha
Anupradana	Anupriya	Anuprya
Anuradha	Anurag	Anuraga
Anuragin	Anuraj	Anurakt
Anuranjana	Anuranjita	Anuratha
Anurodha	Anuruc	Anuruddha
Anurudha	Anurupa	Anusa
Anusikha	Anusobhin	Anusrtasravas
Anusruta	Anutosa	Anutta
Anuvaha	Anuvas	Anuvinda
Anuvitta	Anuvrata	Anuyatri
Anuyayin	Anvagabhanu	Anynaga
Anyuna	Apa	Apacita
Apaghana	Apakunga	Apalala
Apalasin	Apalasuka	Apamanyu
Apan	Apantaratamas	Aparaditya
Aparahnaka	Aparajisnu	Aparimana
Aparimeya	Aparimita	Aparita
Aparoksa	Aparthiva	Aparusa
Apasanka	Apaspati	Apastamba
Apasyu	Api	Apiguna
Apija	Apindra	Apnavana
Apomurti	Appayyadiksita	Aprakarsita
Apramaya	Aprati	Apratima
Apratipa	Apratiratha	Apratirupa
Aprativirya	Apratiyodhin	Apratula
Apsuhomya	Apta	Apu
Apupa	Apurva	Ara
Arabhata	Aracana	Aradhaka
Araga	Arali	Aramanas
Arana	Aranatha	Aranya
Aranyakumara	Arapacana	Arati
Aravind	Aravinda	Aravindan
Aravini	Arbudi	Arca
Arcat	Arcika	Arcita
Ardhendra	Ardra	Arenu
Arganjan	Arghya	Arhana
Arhat	Arhattama	Aridamana
Arihan	Arijit	Arimardana
Arimejaya	Arin	Arisudana
Arita	Ariyappa	Arja
Arjana	Arjava	Arjita
Arjun	Arjuni	Arka
Arkakara	Arkaprakasa	Arkapriya
Arkasa	Arkasmani	Arkin
Arkkaparna	Arksa	Arnava
Aroga	Arokya	Aroon
Arsa	Arsabhi	Arstisena
Arsya	Artaparna	Arthadarsin
Arthasadhaka	Arthasiddhi	Arthvana
Arti	Artiman	Arudra
Arujas	Arul	Arula
Arulamani	Arumugam	Arumugham
Arun	Aruna	Arunakamala
Arunansu	Arunava	Aruni
Arunodaya	Arunopala	Arunsu
Arup	Arva	Arvacina
Arvavasu	Arvind	Arvinda
Arvinder	Arvindra	Arvuda
Arya	Aryabhata	Aryacetas
Aryadeva	Aryaki	Aryakumara
Aryaman	Aryamik	Aryamisra
Aryasura	Aryasva	Aryavarta
Aryendra	Aryika	Asamanjasa
Asamati	Asan	Asanga
Asangas	Asanka	Asankita
Asapurna	Asavaha	Asavijaya
Asecana	Asesa	Ashe
Ashis	Ashish	Ashok
Ashoka	Ashokvardan	Ashrita
Ashutosh	Ashwani	Ashwin
Ashwini	Asiddhartha	Asikrishna
Asiloman	Asim	Asima
Asiman	Asira	Asirisa
Asita	Asitabha	Asitadhanva
Asitadhvaja	Asitasman	Asitotpala
Asjita	Asketa	Askol
Askran	Aslesa	Asma
Asmaka	Asmita	Asmund
Asokadatta	Asokavardhana	Asrava
Asravya	Asrita	Asruta
Asrutavrana	Assar	Astajihva
Astaka	Astaratha	Astavakra
Astika	Astrita	Asu
Asuga	Asula	Asuman
Asumat	Asuratarajasa	Asurayana
Asuri	Asurtarajasa	Asvaghosa
Asvajit	Asvaketu	Asvala
Asvalarana	Asvamedhadatta	Asvamedhas
Asvarya	Asvasena	Asvasiras
Asvathama	Asvattha	Asvatthanarayana
Asvatthi	Asvatyama	Asvavan
Asvin	Asvini	Aswathanarayana
Ataka	Atal	Atala
Atamas	Atandra	Atasa
Athilesa	Ati	Atibhava
Aticanda	Atidatta	Atideva
Atihata	Atima	Atimanita
Atimanusa	Atimanyu	Atimaya
Atindra	Atirathra	Atiratra
Atisa	Atisakra	Atisanda
Atisaya	Atisena	Atish
Atisi	Atisringa	Atisthira
Atisvarya	Atithigva	Ativarcas
Atiya	Atiyama	Atmajnana
Atmananda	Atmavira	Atmaya
Attana	Atul	Atula
Atulavikrama	Atulya	Atyaditya
Atyantika	Atyarati	Audambara
Aukthya	Aurasa	Aurjitya
Autathya	Auttamika	Auttanapadi
Avabhasita	Avabodha	Avacuda
Avadata	Avadha	Avagaha
Avajaya	Avajita	Avajyuta
Avakasa	Avani	Avanibhusana
Avanija	Avanikanta	Avanimohana
Avanindra	Avanipala	Avanisa
Avanisvara	Avantas	Avantivarman
Avapaka	Avaraja	Avarana
Avariyas	Avarodha	Avarokin
Avas	Avasa	Avasathya
Avatsara	Aveesh	Avicala
Avidanta	Avidosa	Avijanata
Avijit	Avijita	Avijna
Avijnatagati	Avik	Avika
Avikala	Aviklava	Aviksipa
Avilasa	Avimukta	Avinasa
Avinash	Avindhya	Avinidevas
Avinna	Avipriya	Aviraga
Avirama	Aviratha	Avirhotra
Avisa	Avita	Avitr
Avitsit	Avrita	Avyaya
Ayasmaya	Ayasya	Ayati
Ayobahu	Ayodhya	Ayuddha
Ayudha	Ayuja	Ayusman
Ayustejas	Ayuta	Ayutajit
Ayutanayi	Ayutasva	Ayutayu
Babhri	Babhrukesa	Babhruloman
Babhruvahana	Babila	Babu
Babul	Babulal	Bacharaja
Bachil	Badal	Badara
Badaridasa	Badarisaila	Baddhananda
Baddhanuraga	Baddharajya	Badrinath
Baduli	Bahadur	Bahubala
Bahudanti	Bahudhana	Bahudhara
Bahugava	Bahuhiranya	Bahujnana
Bahukalyana	Bahuketu	Bahulasavan
Bahuli	Bahumanya	Bahumitra
Bahumulya	Bahupatu	Bahuprada
Bahupriya	Bahurai	Bahuratha
Bahurja	Bahusasta	Bahuskti
Bahusruta	Bahusuvana	Bahuvasin
Bahuvata	Bahuvida	Bahuvidha
Bahuvikrama	Bahuvirya	Bahyasvana
Baijanatha	Baira	Bakanakha
Bakasahavasin	Bakavata	Bakshi
Bakthavatachalam	Bakul	Bakulesa
Bakura	Bala	Balabhandhu
Balabhrt	Balacakravartin	Balachandar
Balachander	Balachandran	Baladeva
Baladeya	Baladhara	Baladhi
Baladhika	Baladhya	Baladitya
Balagopal	Balagra	Balagupta
Balajestha	Balakara	Balakasva
Balakrishna	Balakrishnan	Balakrit
Balaksa	Balakunda	Balamada
Balamadi	Balamitra	Balamodaka
Balamukhya	Balanatha	Balangaka
Balanika	Balaprada	Balaprana
Balar	Balaraja	Balasaheb
Balasalin	Balasena	Balasinha
Balastha	Balasthala	Balasubramaniam
Balasubramanian	Balasubramanyam	Balavan
Balavana	Balavarnin	Balavata
Balavinastaka	Balavira	Balavirya
Balayani	Balayogi	Balayukta
Balayus	Balbinder	Baldev
Balesa	Balhikapungava	Bali
Balibhuja	Balik	Baliman
Balin	Balistha	Balivaka
Balram	Balu	Balula
Balvinder	Balya	Banalinga
Banarasi	Banasankara	Bandana
Bandhujivin	Bandhuman	Bandhupala
Bandin	Banesvara	Banhiman
Banhistha	Bani	Banibrata
Bansi	Bansika	Bapannabhatta
Bappa	Bappaka	Bapu
Bapudeva	Barathi	Barhacandra
Barhaketu	Barhana	Barhanasva
Barhanetra	Barhapida	Barhisada
Barhisapala	Barhistha	Barkha
Barota	Barsati	Baru
Barun	Baryai	Basabi
Basanta	Basava	Basavaraja
Baskarapriya	Basker	Baspa
Basu	Bater	Battaprayaga
Batuka	Beanta	Bekura
Beman	Beni	Beniprasada
Bhabagrahi	Bhabesa	Bhadrabahu
Bhadrabuja	Bhadradeha	Bhadrajatika
Bhadraksa	Bhadrakumbha	Bhadranga
Bhadranukha	Bhadrasa	Bhadrasara
Bhadrasena	Bhadrasila	Bhadrasravas
Bhadrasraya	Bhadrasrenya	Bhadrasva
Bhadratanu	Bhadratmaja	Bhadravaha
Bhadravarman	Bhadrayu	Bhadrika
Bhadriraju	Bhagana	Bhagaratha
Bhagata	Bhagavan	Bhagavana
Bhagavant	Bhagavatiprasada	Bhaguri
Bhagwan	Bhagwandas	Bhagwat
Bhagwati	Bhagyanandana	Bhairavasin
Bhaisora	Bhajana	Bhakta
Bhaktaraja	Bhalachandra	Bhalandana
Bhama	Bhamaha	Bhanaviya
Bhandila	Bhanu	Bhanucandra
Bhanumata	Bhanuratha	Bhanuvarman
Bharanda	Bharat	Bharatabhusana
Bharatendu	Bharath	Bharatha
Bharathi	Bharati	Bharava
Bhargabhumi	Bhargava	Bhargavaka
Bhargavapriya	Bhari	Bharosa
Bhart	Bharti	Bhartrari
Bharupa	Bhasin	Bhaskar
Bhaskaracarya	Bhaskaran	Bhasker
Bhasura	Bhattara	Bhattarahalli
Bhatti	Bhaumendra	Bhaumika
Bhaumiratna	Bhavadatta	Bhavananda
Bhavanatha	Bhavaniprasada	Bhavasagara
Bhavatiga	Bhavesh	Bhavik
Bhavika	Bhavin	Bhavishnu
Bhavitra	Bhavsar	Bhavya
Bheema	Bheemarasetti	Bhiksita
Bhiksu	Bhim	Bhimacandra
Bhimagupta	Bhimanatha	Bhimangada
Bhimapala	Bhimraja	Bhiru
Bhisaj	Bhoganatha	Bhonesa
Bhrajasvata	Bhrajata	Bhrajathu
Bhrajistha	Bhrngara	Bhubhuju
Bhudev	Bhudeva	Bhudhana
Bhugarbha	Bhujabalin	Bhujavirya
Bhujyu	Bhuman	Bhumat
Bhumija	Bhumimitra	Bhuminatha
Bhumindra	Bhumisvara	Bhumitra
Bhumya	Bhunandana	Bhunayaka
Bhunetri	Bhupa	Bhupala
Bhupen	Bhuper	Bhurida
Bhuridaksina	Bhuridyumna	Bhurikirti
Bhurivasu	Bhusakra	Bhusana
Bhushan	Bhusnu	Bhutapala
Bhutasantapana	Bhutinanda	Bhutiraja
Bhutivardhana	Bhuvadvasu	Bhuvan
Bhuvana	Bhuvanacandra	Bhuvanadaka
Bhuvanadhisa	Bhuvanapati	Bhuvanaraja
Bhuvanesa	Bhuvanesh	Bhuvanesvara
Bhuvaneswary	Bhuvapati	Bhuvnesh
Bhuyan	Bidula	Bidyut
Bikarma	Bikas	Bikasa
Billa	Bimal	Biman
Bimbaka	Bimbesvara	Bimbita
Bindra	Bindu	Bindumat
Binduphala	Binota	Binoy
Bipin	Biplab	Bipula
Birbala	Biren	Birendra
Biresvara	Birju	Bisambharnatha
Bishen	Bishwajeet	Bishwajit
Bishwamba	Bisvajita	Biswanath
Biswaroop	Bittu	Bjupinder
Blajit	Bo	Boddr
Bodhamaya	Bodhdisana	Bodhendra
Bodhinmanas	Bogli	Bokil
Bolin	Bommareddy	Bontu
Bora	Borah	Bordoloi
Brahmabhuti	Brahmadanda	Brahmadhara
Brahmadrsa	Brahmadya	Brahmagandha
Brahmagha	Brahmagiri	Brahmajita
Brahmakrita	Brahmakunda	Brahmamurti
Brahmananda	Brahmanida	Brahmaprabha
Brahmaprakasa	Brahmapri	Brahmarasa
Brahmasiras	Brahmasuras	Brahmavarman
Brahmavarta	Brahmayasas	Brahmayuj
Brahmayus	Brahme	Brahmibhuta
Brajamani	Brgala	Brhaddarman
Brhadkaya	Brhajjana	Brhat
Brhatksatra	Bubhutsu	Buddhadatta
Buddhadev	Buddhadeva	Buddhaghosa
Buddhagupta	Buddhaguru	Buddhajnana
Buddhapala	Buddhapalita	Buddharaja
Buddhasena	Buddhasinha	Buddhiprabha
Buddhiraja	Buddu	Budh
Budhana	Budharatna	Budhila
Bukka	Bunda
Cachari	Caitanya	Caitya	Caka	Cakaraka	Cakrasena
Cakravata	Cakrika	Caksana	Caksusya	Camar	Camaraja
Camarvala	Camikara	Campakaprabhu	Campat	Campeya	Campu
Cancu	Candakausika	Candakirana	Candasa	Candidasa	Candrabala
Candrabali	Candrabhana	Candrabhasu	Candrabhuti	Candracarya	Candradatta
Candradipa	Candraduta	Candragomin	Candrajita	Candrakesa	Candrakirana
Candrakrti	Candramanek	Candramani	Candramohan	Candramrta	Candrangada
Candranibha	Candraparvata	Candraprabhava	Candraprakasa	Candraratna	Candrasman
Candrata	Candravallabha	Candravijaya	Candravimala	Candrin	Canduri
Canga	Cangadasa	Caranadasa	Cariman	Carudarsana	Carudatta
Caruhasan	Carusara	Caruvardhana	Catura	Cencanna	Cetana
Cetas	Cetrama	Chabila	Chaila	Chajju	Chand
Chandan	Chander	Chandra	Chandrakant	Chatrapati	Chatravati
Chattradhara	Chavillakara	Chayana	Chhabi	Chirag	Chunda
Chuttur	Cidambara	Cidananda	Cidatmata	Ciddhatu	Cidrupa
Cidullasa	Cidvilasa	Cidvilasini	Cikita	Ciksura	Ciman
Cintamukta	Cintan	Cintaratna	Cintya	Cirajusa	Cirakari
Cirayu	Cirlabhdha	Citaka	Citapati	Citrabhuta	Citrakantha
Citrakarman	Citrasva	Citravasu	Citta	Cittabhoga	Cittaharin
Cittaprabha	Cittaprasadana	Cittaprasanna	Cittaranjana	Cittavata	Cittin
Citvana	Citvata	Civarin	Coksa	Cudakarana	Cudaratna
Cunanda	Cunda	Dabhiti	Dadhyan	Dahana	Dahanavardhana
Daksanila	Daksapati	Daksayana	Daksha	Daksi	Daksina
Daladhisvara	Dalajita	Dalamodaka	Dalapati	Dalijit	Dalip
Daljit	Damati	Damin	Damodar	Damodaragoun	Damodaran
Damya	Danasagara	Danavat	Danavira	Dandaka	Dandamukha
Dandasena	Dandasri	Dandavirya	Dandekar	Danuja	Dara
Darbha	Darbhi	Darpana	Darsana	Darsanapala	Darsaniya
Daruna	Darvanda	Das	Dasadhanus	Dasadyu	Dasajyoti
Dasaketu	Dasasarman	Dash	Dasmata	Dasu	Dasura
Dasuri	Dattadatta	Dattatray	Dattatreya	Dattra	Dattravat
Daulat	Davender	Davindar	Daya	Dayada	Dayala
Dayanidhi	Dayanita	Dayaram	Dayarama	Dayasagara	Dayavira
Dayita	Deb	Debabrata	Debangshu	Debaprosad	Debashis
Debashish	Debasis	Debdan	Debtosh	Deepak	Deepkaran
Deepthi	Deepti	Dehan	Dehesvara	Deo	Deochand
Deodan	Desaka	Desaraj	Desaraja	Dev	Devakumar
Devamaya	Devamisra	Devamuni	Devanabha	Devanaman	Devanand
Devananda	Devanayaka	Devang	Devanirmita	Devanna	Devansa
Devanucara	Devapada	Devapalita	Devapandita	Devaprasada	Devapuspa
Devarajan	Devarama	Devaranya	Devaratha	Devarcaka	Devarha
Devarpana	Devarsi	Devasakha	Devasakti	Devasarasa	Devasativa
Devasiddhi	Devasilpa	Devasisa	Devasista	Devasisu	Devasoma
Devasru	Devasthali	Devasura	Devataras	Devavadha	Devavarman
Devavarsa	Devavesman	Devavi	Devavid	Devayana	Devayasas
Devayukta	Devdas	Devender	Devesh	Devesita	Devesu
Devidasa	Devidatta	Devila	Devin	Devindra	Deviprasad
Deviprasada	Devya	Dhamavat	Dhanajita	Dhananjay	Dhananjaya
Dhanapala	Dhanavanta	Dhandapani	Dhanesha	Dhanirama	Dharabhuja
Dharam	Dharamsi	Dharanija	Dhareshwar	Dharma	Dharmacandra
Dharmacara	Dharmadatta	Dharmadeva	Dharmadhrt	Dharmaghosa	Dharmagopa
Dharmamandhu	Dharmamitra	Dharmamrta	Dharman	Dharmanatha	Dharmanitya
Dharmapala	Dharmaprabasa	Dharmasarathi	Dharmasila	Dharmasimha	Dharmasindhu
Dharmasthavira	Dharmavarna	Dharmavati	Dharmavira	Dharmayasas	Dharmayupa
Dharmendra	Dharmesvara	Dharmistha	Dharmottara	Dhataki	Dhavak
Dhaval	Dhavalacandra	Dhavalapaksa	Dhavita	Dhaya	Dheeraj
Dhillip	Dhirana	Dhircetas	Dhiresa	Dhritiman	Dhrsaj
Dhrsita	Dhrsni	Dhrstaka	Dhrtadaksa	Dhrtadhiti	Dhrtaraja
Dhrtatman	Dhruv	Dhruvasva	Dhruvi	Dhuna	Dhundhi
Dhuninatha	Dhupa	Dhupala	Dhupanravan	Dhurai	Dhvanamodin
Dhyana	Dhyanayogi	Dhyanesa	Dibag	Didhitimat	Didyotisu
Didyu	Digbhraja	Digjaya	Digvijay	Diksin	Diksita
Dileep	Dilip	Dimbha	Dindayala	Dinesh	Dipa
Dipak	Dipankura	Dipen	Dipin	Dipita	Diptatapas
Diptavirya	Dirghadanstra	Dirghadarsana	Dirghasruta	Disnu	Ditaujas
Ditikara	Divekar	Divesh	Divigamana	Divija	Diviksaya
Diviraj	Divoja	Divya	Divyacaksus	Divyadarsana	Divyadeha
Divyaprabhava	Divyendu	Divyesh	Doki	Donkal	Dorai
Doraisvami	Doraiswamy	Drdhadbhakti	Drdhahanu	Drdhaksa	Drdhanga
Drdharuci	Drdhesudhi	Dridhabuddhi	Dronacarya	Drsadvata	Drsika
Drstasara	Drstavirya	Druha	Duddu	Dudha	Dudhanatha
Dulala	Duleep	Dulicandra	Durba	Durdamana	Durdarsin
Durgadasa	Durgadatta	Durgamadhab	Durlabha	Durvarana	Durvartu
Dustara	Dutta	Dvarakadasa	Dvimidha	Dvimurdhan	Dyota
Dyudhaman	Dyujaya	Dyuksa	Dyunisa	Dyutana	Dyutita
Edha	Edhas	Edhatu	Edhita	Ehsan	Ekabandhu
Ekabhakta	Ekachit	Ekada	Ekadhipati	Ekagra	Ekahans
Ekaja	Ekak	Ekala	Ekama	Ekanai	Ekanatha
Ekansa	Ekantin	Ekaraya	Ekasarga	Ekata	Ekatala
Ekatan	Ekatma	Ekayana	Ekayasti	Ekayastika	Ekayavan
Ekdak	Ekisa	Ekodara	Ekta	Ekval	Elu
Enavada	Esana	Esika	Esita	Ettan	Evavada
Gagan	Gaganaghosa	Gaganecara	Ganamnya	Gandhesa	Ganesan
Ganeshram	Ganga	Gangika	Ganin	Ganjan	Ganpat
Garbhaka	Gatha	Gattani	Gaur	Gauradas	Gauramukha
Gauraprabha	Gaurav	Gaurava	Gausra	Gautam	Gavamrita
Gayan	Gayand	Gayaplata	Gayaprasada	Geet	Ghanambu
Ghanaram	Ghanshyam	Gharbharan	Gian	Giani	Giri
Giribhu	Girija	Girijaprasada	Girimana	Giriraj	Gita
Go	Gobind	Gobinda	Gogana	Gokanya	Golaki
Golap	Goman	Goolab	Gopa	Gopakumar	Gopal
Gopaladasa	Gopalakrishna	Gopalakrishnan	Gopalan	Gopalaswamy	Gopalkrishna
Gopalkrishnan	Gopi	Gopicandana	Gopichandra	Gopila	Gopinath
Gopu	Gorla	Gosalaka	Gosarya	Gotam	Goutham
Govardhanen	Govil	Govind	Govinda	Govindagowda	Govindan
Govindappa	Govindarajan	Govindarajulu	Govindaswamy	Grama	Gramakuta
Gramapala	Grht	Guirdeep	Gul	Gulloo	Gulyani
Gunadhara	Gunadhya	Gunagya	Gunamaya	Gunanidhi	Gunasekhara
Gunasraja	Gunavata	Gunayukta	Gunesa	Gunidatta	Gunin
Gunottama	Gurdip	Gurmit	Guru	Gurucarana	Guruda
Gurudasa	Gurudeva	Gurudipa	Gurumel	Gurumita	Gurumukha
Gurumurthi	Gurumurti	Gurunama	Guruprasad	Guruprasada	Gururajan
Gurusarana	Gurusimran	Guruswamy	Guruvacana	Guruvayur	Guruvirna
Gusana	Gutsaka	Gyaneshwar	Gyanprakash	Hakesa	Haliksana
Hansakaya	Hansaraja	Hansini	Hanumant	Haramala	Haramanas
Haran	Haranath	Haranetra	Hararupa	Harasvarupa	Haratejas
Haravira	Harbajan	Harbans	Hardeep	Hari	Haria
Haricapa	Haricarana	Haridhana	Hariharan	Harija	Harikanta
Harikishan	Harilala	Harimbhara	Harinath	Harinder	Hariraja
Harish	Harishkumar	Haritaka	Harjeet	Harjinder	Harjit
Harmage	Harnath	Harnish	Haromohana	Harosit	Harpinder
Harsada	Harsala	Harsamana	Harsavardhana	Harsendu	Harsh
Harsha	Harshad	Harshal	Harshita	Harsoda	Harstia
Hasamukha	Hasaratha	Hasari	Hasta	Hatha	Hatisa
Hatitosa	Havaldar	Hayavahana	Heli	Hem	Hema
Hemai	Hemal	Heman	Hemant	Hemanth	Hemanya
Hima	Himambu	Himavalluka	Himavana	Himayati	Himmat
Hina	Hinadosa	Hiral	Hiralal	Hiranyadanta	Hiranyina
Hiren	Hiresa	Hiru	Hiryur	Hita	Hitaisi
Hitesh	Hitesin	Hitesvara	Honna	Hosang	Hresa
Hridaya	Hridayaja	Hridayangam	Hridayesa	Hridayesvara	Hriman
Hukam	Hulas	Hurditya	Ibhya	Idhma	Idhmavaha
Ijyasila	Iksana	Iksuda	Ikvala	Ila	Ilacandra
Iladhara	Ilanko	Ilaspada	Ilaspati	Ili	Inakanta
Inan	Inder	Inderjit	Inderpal	Indrabala	Indradatta
Indradu	Indraghosa	Indragopa	Indraja	Indrapramati	Indrapriya
Indrarajan	Indrasana	Indrasita	Indrasura	Indrasvat	Indratan
Indravadana	Indrayan	Indrayatana	Indrayava	Indrinika	Indudeep
Indumathy	Indumukhi	Inganam	Ira	Iravan	Iravata
Irimpu	Irith	Isan	Isanam	Isat	Isav
Isayu	Ishao	Ishwaran	Ismin	Isrita	Istaka
Istika	Isuka	Isvaracandra	Isvaraprasada	Isvasa	Itar
Iyam	Jabhar	Jadhav	Jagacitra	Jagad	Jagadananda
Jagadbala	Jagadish	Jagajiva	Jagan	Jaganmani	Jagannath
Jagannathan	Jagannidhi	Jaganu	Jagaprtia	Jagara	Jagarupa
Jagat	Jagatipati	Jagatjita	Jagatjiva	Jagatkarna	Jagatprakasa
Jagavanta	Jagdish	Jagganathan	Jagisa	Jagjit	Jagmi
Jagnu	Jaguri	Jai	Jaibhusana	Jaideep	Jaidev
Jaideva	Jaidhvani	Jaigata	Jaighosa	Jaigupta	Jaikara
Jaikirti	Jaikrta	Jaimalla	Jaimangala	Jaipida	Jairaja
Jairasa	Jairath	Jaisekhara	Jaishankar	Jaisinharaja	Jaisisa
Jaisnava	Jaistambha	Jaitanga	Jaitrama	Jaivaha	Jaivanta
Jaivata	Jaivira	Jaja	Jajal	Jajhara	Jalada
Jalambara	Jalancala	Jalanidhi	Jalarka	Jalasa	Jaman
Jambunathan	Jamshed	Jana	Janacandra	Janadhipa	Janaka
Janakiraman	Jananatha	Janapadin	Janapalaka	Janapati	Janarajan
Janardan	Janardhan	Janardhanan	Janasruta	Janaswami	Janava
Janendra	Janesa	Janésvara	Jangi	Janistha	Janita
Janmajyestha	Janu	Janya	Jaritri	Jasalina	Jasamita
Jasapala	Jasaraja	Jasavanta	Jasavira	Jaswant	Jaswinder
Jata	Jatacira	Jatakara	Jatasaya	Jatasila	Jatin
Jatinder	Jatindhara	Jatusthira	Jatya	Javagal	Javanila
Javeed	Javin	Javistha	Jawahar	Jawahara	Jawaheer
Jay	Jaya	Jayachandran	Jayachnadran	Jayadev	Jayadevan
Jayakar	Jayakrishna	Jayakumar	Jayalakshm	Jayan	Jayani
Jayant	Jayanta	Jayanth	Jayanthi	Jayaram	Jayaraman
Jayaratha	Jayashankar	Jayashree	Jayasnava	Jayasvamin	Jayenda
Jayin	Jayisnu	Jayprakash	Jaywant	Jeet	Jeetender
Jeman	Jenya	Jetva	Jeyakesavan	Jhawar	Jhilmit
Jhinka	Jigisu	Jinaduraja	Jinda	Jinraj	Jit
Jita	Jitender	Jitendra	Jitinder	Jitrendra	Jitrindra
Jitvan	Jivabhuta	Jivadeva	Jivaja	Jivana	Jivanadhara
Jivanatha	Jivanikaya	Jivaratna	Jivatha	Jivavijaya	Jiwani
Joginder	Johar	Josa	Josita	Joydeep	Joyendu
Joyjeet	Jrashesh	Jugala	Juggy	Jugnu	Justa
Juvas	Jvala	Jvalanmani	Jvalaprasada	Jvalka	Jyayas
Jyotibhasin	Jyotin	Jyotindra	Jyotiprakasa	Jyotirbhaga	Kacangala
Kacapa	Kacesvara	Kacima	Kadambanila	Kadambi	Kaditula
Kagni	Kahola	Kailash	Kaivalya	Kajal	Kajjala
Kakanda	Kakila	Kakodara	Kakubjaya	Kakudman	Kakunda
Kala	Kalabhasin	Kalabhiti	Kalakanja	Kalakuta	Kalal
Kalale	Kalamurti	Kalankura	Kalapriya	Kalaraja	Kalasinha
Kaldhuta	Kalhana	Kalhara	Kalicarana	Kalidas	Kalidasa
Kalijan	Kalila	Kalita	Kallola	Kalmali	Kalpana
Kalpanath	Kalpanatha	Kalpesa	Kalpesh	Kalpita	Kalskandha
Kalvik	Kalya	Kalyan	Kalyanaraman	Kalyanasarman	Kalyanasundaram
Kalyanavarman	Kalyanavata	Kalyanin	Kam	Kamadev	Kamakshi
Kamal	Kamalabuddhi	Kamalahasa	Kamalakara	Kamalamaya	Kamalanayana
Kamalanetra	Kamalodaya	Kamaraja	Kamarajan	Kamarupa	Kamasakha
Kamasrama	Kamayani	Kambadur	Kamboja	Kamesh	Kameswar
Kameswara	Kami	Kamika	Kamlesh	Kamuka	Kanak
Kanakadatta	Kanakakanta	Kanakambujam	Kanakamya	Kanakarasa	Kanakendu
Kanakraj	Kanala	Kanca	Kancuka	Kancukita	Kandala
Kandarpaketu	Kanikaraja	Kaninaka	Kanjam	Kanjavadana	Kanji
Kankala	Kanksita	Kannan	Kant	Kanta	Kanthamani
Kanti	Kantida	Kanu	Kanvala	Kanvar	Kanwal
Kapalabhrt	Kapalaketu	Kapil	Kapilan	Kara	Karacura
Karajala	Karakasa	Karambha	Karan	Karanam	Karanja
Karapagam	Karavinda	Karikrsna	Karin	Karisnu	Karkasa
Karmacandra	Karmanya	Karmarkar	Karmasura	Karmatman	Karmavajra
Karmavira	Karmendra	Karnadhara	Karnaka	Karnamukha	Karnavira
Karnikara	Karniki	Karpuratilaka	Kartara	Karthik	Karthikeyan
Kartik	Kartikeya	Karu	Karunakara	Karunamaya	Karunamoorthy
Karunanidhi	Karunesh	Kasara	Kashmiri	Kashyap	Kasi
Kasilingam	Kasin	Kasinathan	Kasirama	Kasisnu	Kataka
Katama	Katamaraja	Kathita	Katriyar	Katta	Katti
Katumbi	Katyayani	Kavana	Kavi	Kavibhusana	Kavisvara
Kavita	Kavitha	Kavitva	Kerkhi	Ketan	Ketana
Ketika	Ketita	Ketubha	Ketubhuta	Ketutra	Kevala
Kevalin	Keya	Keyur	Keyura	Khamurti	Khanaka
Khanjana	Kharag	Khatra	Khatri	Khawaja	Khayali
Khetan	Khettry	Khevanaraja	Khokhun	Khullana	Khusbu
Khusila	Khusirama	Khusmana	Khusvanta	Kinjata	Kiran
Kirana	Kirba Ran	Kirika	Kirin	Kirit	Kirita
Kirpal	Kirtana	Kirtenya	Kirti	Kirtibhusana	Kirtideva
Kirtidhara	Kirtimaya	Kirtita	Kisalaya	Kishan	Kishen
Kishor	Kishore	Kisku	Kisni	Kistikumara	Kistna
Komal	Komala	Koti	Kotijit	Kotisvara	Kotta
Kovalan	Kovida	Krakaca	Krama	Krantivira	Kratubhuj
Kratukarana	Kratumaya	Kratupati	Kratvamagha	Kratvanga	Kris
Krish	Krishan	Krishanin	Krishnakumar	Krishnamachari	Krishnamoorthy
Krishnamurthi	Krishnamurthy	Krishnamy	Krishnan	Krishnapillai	Krishnaraju
Krishnaswami	Krishnaswamy	Kriyavidhi	Krpala	Krpana	Krpanaka
Krpananda	Krpasagara	Krsnakanta	Krsnamitra	Krsnantara	Krsnika
Krsniya	Krtahasta	Krtakama	Krtalaksana	Krtamukha	Krtanjali
Krtsna	Krtsnavid	Krtya	Ksama	Ksamabhuj	Ksamamitra
Ksaman	Ksamapati	Ksambhuj	Ksantu	Ksapavana	Ksaya
Ksayana	Ksayata	Ksema	Ksemamaya	Kshama	Kshitij
Ksipanu	Ksirasukla	Ksitendra	Ksitibhuj	Ksitiksita	Ksitilavabhuj
Ksitinatha	Ksitindra	Ksitipati	Ksitipuruhuta	Ksitisa	Ksonideva
Kuberabandhu	Kucandana	Kudhara	Kuhuk	Kujapa	Kukila
Kulabhusana	Kuladeva	Kuladipa	Kuladipaka	Kulaja	Kulamani
Kulapati	Kulatilaka	Kulavira	Kuldeep	Kuldip	Kulin
Kulisaya	Kulluka	Kulvinder	Kulvir	Kumar	Kumara
Kumaragupta	Kumaran	Kumarasamy	Kumaresan	Kumarila	Kunamneni
Kundam	Kundan	Kunjita	Kunju	Kunsa	Kuntaka
Kupati	Kuppusamy	Kuppuswamy	Kurcika	Kuruk	Kuruntika
Kusalin	Kush	Kushan	Kushwah	Kushwaha	Kushwant
Kusumakar	Kusumakara	Kusumasekhara	Kusumasrestha	Kusumoda	Kusumojjvala
Kusumojvala	Kutilagesa	Kuvala	Kuvalyesa	Kvana
Lakshman	Lakshmana	Lakshmanan	Lal	Lala	Lall
Laloo	Laxman	Leil	Loganathan	Lookman	Madanraj
Madhav	Madhavan	Madhavrao	Madhuveer	Mahavir	Mahendra
Mahesh	Maheshwari	Maheswari	Mahinder	Malkiat	Mani
Manik	Manilal	Manjinder	Manjit	Manoj	Manoja
Manoo	Manooj	Manu	Massem	Mathur	Mathura
Milkesh	Min	Mina	Mohan	Mohana	Mohanakrishnan
Mohankumar	Mohinder	Mohindra	Moosa	Motilal	Mrinal
Mukesh	Mukul	Murali	Muralidaran	Muralidhar	Muralidhara
Muru	Nabil	Nagendra	Naina	Nalin	Nand
Nanda	Narain	Narayan	Narayana	Narayanan	Naren
Narendra	Naresh	Narinder	Narotham	Narottam	Nataraj
Nataraja	Natarajan	Natraj	Natrajan	Naveen	Navin
Navjot	Nayan	Nichit	Nikhil	Niranjan	Nitya
Nityanand	Om	Omar	Palla	Pallab	Paramananda
Paramjit	Paras	Parasher	Paresh	Parvathi	Parvatiyar
Patel	Peshora	Piara	Pirthee	Pitambar	Prabha
Prabhakar	Prabhakaran	Prabhu	Prabhukumar	Prabhusha	Prabodh
Pradeep	Prafel	Prakash	Pramod	Pran	Pranav
Pranay	Prasad	Pratap	Prathapan	Pravin	Prem
Prema	Premanand	Premchand	Pritam	Pururavas	Purushotham
Purushothaman	Purushottam	Qhanraj	Radhakrish	Radhakrishna	Radhakrishnan
Raghav	Raghava	Raghavachary	Raghavan	Raghu	Raghunand
Raghuvir	Rahul	Raivata	Raj	Raja	Rajam
Rajanikant	Rajeeb	Rajeev	Rajendar	Rajender	Rajendra
Rajendran	Rajesh	Rajeshh	Rajeshkumar	Rajeshwari	Rajeswara
Rajib	Rajik	Rajil	Rajindar	Rajiv	Rajjun
Rajkumar	Rajluxmi	Rajminder	Rajneesh	Rajni	Rajnish
Rajpal	Raju	Ram	Rama	Ramabhadran	Ramachander
Ramachandra	Ramachandramurthy	Ramachandran	Ramakant	Ramakanth	Ramakota
Ramakrishna	Ramakrishnan	Ramani	Ramdas	Ramesh	Rameshchandra
Rameshwar	Rameysh	Ramgopal	Ramindar	Raminder	Ramkrishna
Ramnarayan	Ramnath	Ramu	Rana	Ranjit	Ranjith
Ras	Rasesh	Ratan	Rati	Ratilal	Ravi
Ravichandran	Ravider	Ravinder	Ravindiran	Ravindra	Ravindran
Ravishankar	Renu	Rohan	Rohit	Romesh	Roshan
Runjeet	Rupchand	Sabu	Sachdev	Sakar	Samant
Sanjay	Sanjeev	Sanjiv	Sanjog	Sanjoy	Sankar
Sankara	Santu	Saran	Sarang	Saravanan	Sarda
Sardar	Sardul	Sateesh	Sateesha	Satish	Satishkuma
Satyajit	Saurabh	Saurav	Sefreen	Senthil	Seth
Sewa	Sewam	Shaaban	Sham	Shamabhat	Shambu
Shankar	Shanker	Sharma	Shashi	Sheetal	Shekar
Shekara	Shekhar	Sheth	Shital	Shiv	Shivaji
Shripati	Shyam	Shyamal	Siddarth	Siddharth	Siddhartha
Sitaram	Sivarasa	Sohan	Sonal	Sonali	Sridhar
Srikant	Srikanta	Srikanth	Srinivas	Srinivasa	Srinivasan
Srinivasiah	Sriram	Subhadra	Subhadran	Subhash	Sudeep
Sudhin	Sudhir	Sultan	Sumanjit	Sundar	Sundara
Sundaram	Sunder	Sunderi	Sunil	Suresh	Surinder
Suseela	Susheel	Susheela	Sushil	Susila	Swapan
Swapna	Tara	Tarun	Taruna	Tej	Teja
Tejal	Teji	Teju	Thakur	Tulasi	Tulsi
Umashankar	Uttam	Vasan	Vasant	Vasanta	Vasantha
Vasanthi	Vasantraj	Vasu	Ved	Vedi	Venkat
Venkata	Venkatesh	Venkatesha	Vidya	Vidyahar	Vij
Vijai	Vijay	Vijaya	Vikas	Vikram	Vikrant
Vimal	Vinay	Vinaya	Vinayak	Vinayaka	Vindoo
Vinod	Vishnu	Vishwanand	Vishwanata	Vishwanath	Vishy
Yadav	Yadawa	Yash	Yashpal	Yog"""

let hfemale = """Aanchal	Aarthika	Aarti	Abburi	Abha	Abhati
Abheri	Abhibha	Abhidhya	Abhidya	Abhigurti	Abhijiti
Abhijna	Abhikya	Abhilasa	Abhilasin	Abhiniti	Abhipri
Abhipriti	Abhipuspam	Abhiraksa	Abhirati	Abhirka	Abhiruci
Abhisri	Abhisvara	Abhiti	Abhivadaka	Abhivibha	Abhraganga
Abhramu	Abhranti	Abhrayanti	Abhumukhi	Abja	Abjini
Aboli	Acala	Acaryanandana	Acaryaputra	Acaryatanaya	Acchoda
Achit	Acira	Adapa	Adarna	Adevi	Adhimuhya
Adhrsya	Adibuddha	Adilakshmi	Adita	Aditya	Adityabandhu
Adlakha	Adrika	Adrsyanti	Adusumilli	Advaitavadini	Agnajita
Agnayi	Agneyi	Agnimukhi	Agnivardini	Agraja	Agrayi
Ahalya	Ahana	Ahanti	Ahdita	Ahi	Ahilya
Ahimsa	Ahladita	Ahu	Ahuti	Aiksvaki	Aisvarya
Aiyah	Aja	Ajai	Ajamukhhi	Ajani	Ajanta
Ajara	Ajatashatru	Ajathya	Ajaya	Aji	Ajinder
Ajitha	Ajmani	Ajoy	Ajwani	Akaash	Akaliki
Akalka	Akanksa	Akasadipa	Akasaganga	Akasi	Akella
Akkina	Akkiraju	Akolekar	Akranti	Akriti	Akrti
Aksasutra	Aksi	Aksiti	Akupara	Akuti	Alagar
Alaka	Alakaravati	Alambusa	Alamelu	Alapini	Alisa
Alishah	Alka	Alkesh	Alla	Alpa	Alpana
Alwar	Amal	Amala	Amani	Amaraja	Amaranagana
Amaratatini	Amaravati	Amardeep	Amari	Amarta	Amati
Amavasya	Amba	Ambady	Ambala	Ambali	Ambalika
Ambaraprabha	Ambatipudi	Ambaya	Ambhoji	Ambhojini	Ambi
Ambika	Ambrish	Ambuj	Ambujakshi	Ambujanana	Ambumati
Ambupadma	Ambuvahini	Ameena	Ameet	Ami	Amiksh
Amina	Amisa	Amita	Amitesvari	Amiti	Amiya
Ammu	Amodini	Amoghaksi	Amohanika	Amramanjari	Amrit
Amrita	Amritama	Amritansh	Amritendu	Amruth	Amsel
Amutha	Anabhra	Anadya	Anaga	Anamika	Anamra
Ananda	Anandadevi	Anandalakshmi	Anandamayi	Anandaparna	Anandaprabha
Anantalakshmi	Ananya	Anati	Anavadya	Anavi	Anay
Andala	Andika	Aneesh	Angad	Angada	Angaja
Angana	Angaravati	Angarita	Anguri	Anhati	Anhiti
Anil	Anila	Anima	Aninda	Anindini	Anindita
Anindya	Anisa	Anita	Anjalika	Anjanam	Anjasi
Anji	Anjini	Anjna	Anju	Anjum	Anjuman
Ankan	Ankita	Ankitha	Ankolika	Annamalai	Annanya
Anni	Anokhi	Anoma	Anritam	Ansuiya	Ansumala
Ansumati	Antika	Antini	Antur	Anu	Anubha
Anugita	Anuka	Anukanksa	Anula	Anulekha	Anuli
Anumodita	Anunayika	Anunita	Anuniti	Anupallavi	Anuprahba
Anupriya	Anuradha	Anurakti	Anurati	Anurima	Anusara
Anushree	Anusna	Anusobhini	Anusri	Anutapta	Anvakriti
Anvita	Anviti	Anya	Apaciti	Apaga	Apala
Aparajita	Aparananda	Aparna	Apaya	Apeksita	Apti
Apurani	Apurva	Aqsa	Ara	Aradhana	Aradhita
Araja	Arani	Arasu	Arati	Aravamudan	Aravas
Aravindini	Arcana	Archana	Archna	Arcismati	Ardhaganga
Arhana	Arhantika	Arihan	Arikta	Arindam	Aripra
Arja	Arkasuta	Arnab	Aroga	Arohi	Arpana
Arpita	Arthana	Artika	Aruja	Aruksita	Aruna
Arunabha	Aruni	Arunika	Arunima	Arupa	Arusi
Arvanti	Arya	Aryamani	Asa	Asadhika	Asali
Asanjini	Asanni	Asija	Asika	Asikni	Asira
Asirvatham	Asisa	Askini	Asmaki	Asmati	Asmi
Asna	Asokari	Asta	Astha	Asthula	Asti
Astriti	Asura	Asutosh	Asvattha	Asvika	Atchut
Atchuta	Atharvan	Athavale	Athawale	Athreya	Atibala
Atimoda	Atiriya	Atmadhika	Atmajyoti	Atmodbhava	Atyuha
Aurjitya	Ausinari	Auvvayar	Avabha	Avachat	Avadhanam
Avajiti	Avalur	Avanati	Avani	Avanthika	Avanti
Avantika	Avantivati	Avaraja	Avasarala	Avatansa	Avatika
Aviral	Avisi	Avisya	Ayati	Ayodhika	Ayugu
Babhru	Babita	Bachendri	Badani	Badarayani	Badari
Badithe	Badsah	Bagade	Bagesri	Bagga	Bahughanda
Bahuli	Bahulika	Bahumati	Bahuratna	Bahvisvara	Baiju
Baindur	Bakavati	Bakula	Bakulamala	Bakulika	Bala
Balada	Balaja	Balaji	Balakunda	Balandhara	Balapuspika
Balasandhya	Balavati	Balki	Banaganga	Banasri	Banasuta
Bandaru	Bandhini	Bandhumati	Bandhupriya	Bandhura	Bandlish
Bano	Bansuri	Baratam	Barhayita	Barhina	Barhisa
Basanti	Basha	Baskaran	Basude	Basvangoud	Batakrishna
Bawa	Bawara	Baxi	Bayya	Bbusoowy	Beddhu
Beena	Beerud	Behari	Bekkem	Bela	Beli
Bellamkonda	Bellare	Belur	Bemra	Betanabhatla	Bettadapura
Bettadpur	Bhabani	Bhabra	Bhadrabhusana	Bhadramukhi	Bhadrarupa
Bhadrasvapna	Bhadravalli	Bhadrika	Bhagavanti	Bhagavatula	Bhagra
Bhagya	Bhaina	Bhalli	Bhamidipati	Bhamini	Bhanap
Bhanavi	Bhandana	Bhanot	Bhanuja	Bhanumati	Bhanupriya
Bhanusri	Bharadvaji	Bharadwaj	Bharani	Bharati	Bharava
Bhardwaj	Bhargavi	Bharti	Bharu	Bhasi	Bhati
Bhattini	Bhauma	Bhavaja	Bhavana	Bhavangama	Bhavanika
Bhavanti	Bhavapuspa	Bhaviki	Bhavitra	Bhimavarapu	Bhishma
Bhogya	Bhomira	Bhoopathi	Bhraji	Bhramambika	Bhramarika
Bhrami	Bhrigu	Bhrngari	Bhubaneswar	Bhumralkar	Bhurji
Bhusa	Bhuva	Bhuvanagiri	Bhuvanamati	Bhuvanesani	Bhuvani
Bhuvis	Bidalika	Bijaharini	Bijaksara	Bijal	Bijli
Bijoy	Biju	Bikramjit	Billoo	Bimbi	Bimbini
Bina	Bindiya	Bindu	Bindumati	Bindurekha	Binita
Birewar	Bisala	Bisaria	Bishnu	Bishu	Bisini
Biswanath	Bodhini	Boppana	Boreda	Brahmanjali	Brhadyuti
Brhanmati	Bridgnandan	Brij	Brijesh	Brinda	Buddhidevi
Buddhimatika	Bula	Bulusu	Burjiz	Cahana	Caitali
Caitri	Cakramardika	Caksani	Caksusi	Canaya	Candakirana
Candani	Candrabala	Candragauri	Candragolika	Candrakali	Candramati
Candramukha	Candramukhi	Candrasila	Candrasri	Candrasubha	Candravadana
Candravasa	Candri	Candrima	Candrupa	Carani	Carnapurna
Carubala	Carucitra	Carudarsana	Carulata	Carulocana	Carusila
Carutama	Casukhela	Casula	Caturika	Cauvery	Cchandra
Celana	Cesta	Cetana	Chadaga	Chaitanya	Chaitra
Chakradhar	Chakrapani	Chalamala	Chalana	Chalasani	Challa
Chand	Chanda	Chandra	Chandrakanta	Chandralekha	Chaund
Chava	Chavi	Chhaya	Chinna	Chinta	Chintala
Chintam	Chinya	Chitra	Chitta	Chundra	Cinta
Citrai	Citrajyoti	Citrali	Citramaya	Citramayi	Citrarati
Citrini	Citrita	Citta	Cittii	Cudala	Cumba
Cumban	Cunni	Daga	Dahanolka	Daksayaninya	Dalaja
Damodari	Dandapani	Danti	Darpanika	Darsatasri	Dasamalika
Dattadevi	Datti	Dayadi	Dayanvita	Dayavati	Dayita
Dayumnahuti	Deena	Deepali	Deepika	Desna	Devaki
Devamani	Devamati	Devamayi	Devanganga	Devapratima	Devaradhana
Devarapalli	Devasmita	Devavani	Devavati	Devaviti	Devayosa
Devi	Devika	Deviki	Devina	Dhamani	Dhanavati
Dhanistha	Dhanvanya	Dhanyamala	Dharmini	Dhilati	Dhita
Dhiti	Dhrtavati	Dhulika	Dhulipala	Dhulipalla	Didhi
Dinaprabha	Dipa	Dipakalika	Dipaksi	Dipali	Dipamala
Dipanjali	Dipanwita	Dipavali	Dipmani	Dipra	Dipsikha
Disti	Divija	Divolka	Divyadevi	Divyajyoti	Divyakriti
Drdhamati	Drgbhu	Drsika	Druhi	Druti	Dulari
Durgila	Duvasvati	Dyotana	Dyuksa	Edha	Ekabhakti
Ekacarini	Ekadasi	Ekadhana	Ekaja	Ekakini	Ekamati
Ekangika	Ekanta	Ekisa	Eloksi	Enaksi	Eneela
Eni	Enipada	Esa	Esanika	Eta	Etaha
Eti	Gaganecara	Gajagamani	Gajagati	Gajalakshmi	Gajamukta
Gajapathi	Gajaweera	Gajra	Gamati	Gamin	Ganda
Gandhaja	Gangangini	Ganjan	Ganmanya	Garbhagrha	Gargi
Gathika	Gaurang	Gavah	Gayanti	Gayathri	Gayatri
Gayatrini	Geeta	Geetha	Geethanjali	Gera	Gesna
Ghanivalli	Ghanivallika	Ghosavati	Ghugari	Girisma	Girni
Gita	Gitali	Gitanjali	Giti	Gitika	Gopa
Gopabala	Gopaja	Goparasa	Gorocana	Gouri	Gowri
Gulal	Gulika	Gulmini	Gunamaya	Gunavini	Gunca
Guncaka	Gundu	Guneeta	Gunita	Gunjan	Gunnika
Gunti	Gunvati	Gupti	Gurdiya	Gurnika	Gurti
Gurucarana	Guruda	Gurudeva	Gurudipa	Gurumita	Gurumukha
Gurunama	Guruprasada	Gurusarana	Guruvacana	Guruvira	Gutika
Hala	Halima	Haliman	Hansanadini	Hansanandini	Hararvarupa
Haravali	Haribhadra	Harinaksi	Hariprita	Harisri	Harmya
Harsala	Harsavina	Harsi	Harsita	Harsumati	Hasika
Hasumati	Heena	Hela	Hemaksi	Hemamalini	Hemangini
Hemanti	Hemaprabha	Hemavarna	Himasveta	Hiya	Hrada
Ibha	Ibhi	Iccha	Icchavati	Iditri	Iha
Ihita	Ijya	Iksa	Iksenya	Iksulata	Ilaksi
Ilesa	Ilika	Ilisa	Inaksi	Indali	Inderjit
Indira	Indrabala	Indrabha	Indrabhattarika	Indrahuti	Indranilika
Indu	Induvadana	Inika	Ipsa	Ipsita	Iraja
Iravati	Irijaya	Irika	Isa	Isanika	Isika
Istara	Itkila	Iya	Jabala	Jagatna	Jagavi
Jagrati	Jagrti	Jagruti	Jailekha	Jaimala	Jaiman
Jaiprbha	Jaishree	Jaisila	Jaisudha	Jaivanti	Jaivati
Jalabalika	Jalajaksi	Jalajini	Jalalata	Jalambika	Jalanili
Jalarnava	Jalavalika	Jallata	Jama	Jambhalika	Janabalika
Janaki	Janamohini	Janasruti	Janhita	Janki	Januja
Jasalina	Jasarani	Jaswinder	Jatarupa	Jatila	Jatukarna
Jaya	Jayalaksmi	Jayanti	Jayashree	Jayita	Jayitri
Jeendan	Jhala	Jharna	Jhatalika	Jhillika	Jhilmil
Jigisa	Jindan	Jitya	Jivapuspa	Jivika	Jogu
Josa	Josya	Jurni	Justi	Jutika	Jvalita
Jvalitri	Jyota	Jyoti	Jyotiranika	Jyotsna	Jyotsni
Kadhapriya	Kahini	Kailash	Kajal	Kajri	Kakali
Kaksi	Kalahapriya	Kalakanya	Kalamali	Kalandika	Kalapini
Kale	Kalia	Kaliappa	Kalidindi	Kalipi	Kalli
Kalmesika	Kalpana	Kalpavati	Kalyani	Kama	Kamadyu
Kamala	Kamalah	Kamalalocana	Kamalanayani	Kamalata	Kamaleksana
Kamalika	Kamana	Kamandaki	Kamarekha	Kamayani	Kamini
Kamita	Kamlesh	Kamma	Kamna	Kamni	Kamra
Kamya	Kana	Kanakalata	Kanakamanjari	Kanakamudra	Kanakasundari
Kanakavali	Kanakavalli	Kanakvi	Kanam	Kananabala	Kancanabha
Kanchana	Kandarpabala	Kandhara	Kangana	Kani	Kania
Kanici	Kanika	Kanin	Kanistha	Kanita	Kanjari
Kanjira	Kankan	Kankanika	Kanksa	Kanksini	Kannika
Kanta	Kanti	Kanya	Kanyala	Kanyana	Kanyaratna
Kapadia	Kapardika	Karabhoru	Karia	Karika	Karisma
Karisni	Karkari	Karmistha	Karpani	Karpuri	Karsna
Karttiki	Karuna	Karunya	Kasturba	Kasturi	Kasturika
Kasu	Kausalika	Kausalya	Kavika	Kavita	Keyurin
Khatu	Kheli	Kilasla	Kinjala	Kiranamayi	Kirmi
Kishori	Kisori	Kiya	Kour	Kranti	Krpa
Krsana	Krsi	Krsnan	Krsni	Ksa	Ksamamati
Ksamya	Ksanada	Ksatriyani	Ksirasagara	Ksunu	Kuladevi
Kulanari	Kulangana	Kumari	Kumaria	Kumud	Kumudika
Kumudini	Kundamala	Kundini	Kunja	Kunjalata	Kunsi
Kurangaksi	Kurira	Kusumanjali	Kusumavati	Kusumaya	Kusumita
Kuthodari	Kuvalayadhrs	Kuvalayini	Kuvalayita	Kuvarini	Laboni
Laduri	Lakshmi	Lakshmikantan	Lakshminarashimhan	Lakshminaraya	Lakshminarayanan
Laxmi	Laxmikant	Laxminarasimha	Leela	Leena	Lela
Madhavi	Madhu	Madhucchandra	Madhukar	Madhulika	Madhumati
Madhumita	Madhumitra	Madhur	Madhuri	Madhusree	Madhusudhan
Madhusudhana	Madhusudhanan	Makara	Mala	Malar	Malathi
Malati	Malini	Mandara	Mangla	Manglani	Mani
Manika	Manish	Manisha	Meena	Meera	Mehadi
Mena	Menaka	Mesha	Meshal	Minda	Mira
Mita	Mohana	Mohani	Mohini	Mohit	Munakshi
Mungela	Natesa	Natisa	Nayantara	Nimai	Nimi
Nimmi	Ninderjit	Nisha	Nishchint	Padma	Padmaja
Padmavati	Padmini	Pandita	Pari	Parimal	Parimala
Parmeshwarii	Parminder	Paro	Pinga	Pollyam	Pom
Poonam	Poornima	Pratibha	Preia	Prema	Premila
Premlata	Priya	Promila	Pupul	Purna	Purnima
Pushpa	Puspa	Radhe	Radhika	Rajalakshmi	Raje
Raji	Rajni	Rakesh	Rani	Rati	Reena
Renu	Rita	Ritu	Rochana	Rohana	Rohena
Rupinder	Sahana	Sahera	Sakara	Sakari	Sakyamuni
Sala	Sandhya	Santosh	Sarala	Saroj	Saroja
Sarojini	Sateesh	Satheesh	Sathya	Satish	Satya
Satyam	Saura	Savita	Savitha	Savitri	Seema
Seeta	Seetha	Sepatha	Shabana	Shaila	Shakti
Shakuntala	Shalini	Shanta	Shantanu	Shanthi	Shanti
Sharad	Sharada	Sharath	Sharmila	Shashi	Sheela
Sheila	Shoba	Shobha	Shobhana	Shobhna	Shobu
Shreya	Shri	Shyama	Shyamala	Shyamani	Siddartha
Sidharth	Smriti	Sneh	Soochet	Sri	Suja
Sujata	Sujatha	Sumant	Sumanth	Sumantra	Sumantu
Sumathi	Sumati	Sumit	Sumita	Sumitra	Sumitro
Suneet	Suneetha	Sunit	Sunita	Sunitha	Suniti
Surajit	Sureshta	Surjit	Sushila	Sushma	Swaran
Tara	Uma	Umi	Urmila	Urmilla	Urvashi
Urvasi	Usha	Vanmala	Vasanta	Vasundara	Vasundhara
Vimala	Vimi	Vimla	Zubeida"""

let hlast = """Adani	Advani	Agarkar	Agarwal	Agrawal	Ahsen
Amarnath	Amra	Amroliwallah	Anand	Ankola	Apte
Arasaratnam	Aron	Baboor	Badesha	Bahl	Bai
Balakrishnan	Banerjee	Banker	Barendran	Barot	Battacharjee
Bedi	Behari	Bhagwat	Bhagyamma	Bhanghoo	Bhanjee
Bhaskar	Bhatt	Bhattacharya	Bhatti	Bhoola	Bipen
Bisht	Biswas	Bonjani	Boparai	Buchar	Buhpathi
Bux	Cansai	Chakrabarti	Chandak	Chandan	Chandar
Chande	Chander	Chandiramani	Chandna	Chandra	Chandrakala
Chandramouleeswaran	Chandramouli	Chandran	Chandrark	Chandrasekar	Chandrasekaran
Chandrasekhar	Chandrasekharan	Chandrashaker	Chandrashekar	Channarayapatra	Chapal
Charan	Charu	Chaterju	Chatterjee	Chatterji	Chaudhari
Chaudhry	Chaudhury	Chauhan	Chawd	Cheenu	Chella
Chellaiah	Chellappa	Chellappan	Chengelpet	Chennapragada	Cheran
Cherukuri	Cherupara	Chet	Chetan	Chetana	Chethan
Chetlapalli	Chhachhi	Chhavvi	Chheda	Chiba	Chidambaram
Chidamber	Chikodi	Chinmay	Chinnakannan	Chinnappan	Chippada
Chirag	Chirimar	Chitnis	Chitrangda	Chittibabu	Chittoor
Chittor	Chitturu	Chivukula	Chohan	Choudhari	Choudhary
Choudhury	Choughoy	Chowdhury	Chowdry	Chudasama	Contractor
Dalmiya	Dama	Darisipudi	Darsha	Daruka	Daryapurkar
Dasari	Dasgupta	Datar	Datla	Datta	Davuluri
Dawar	Dehiya	Deivan	Deol	Desai	Dhadda
Dhaliwal	Dharuna	Dhatri	Dhawan	Dhiri	Dhrtiman
Dhruba	Dhupam	Dhurvasula	Dibyendu	Diggavi	Dinath
Dinkar	Dinkerrai	Divecha	Diwan	Dosanjh	Dravid
Dristi	Dua	Duleepsinhji	Durai	Duranjaya	Durjaya
Durmada	Duvvoori	Dwijen	Edulbehram	Ekachakra	Eknath
Elango	Elayavalli	Emankum	Emankumar	Engineer	Eswara
Eswarapu	Gadde	Gade	Gadepalli	Gaekwad	Gahlot
Gajaren	Gajendra	Gajraj	Gala	Gambhir	Ganapathiraman
Ganapathy	Ganesh	Ganeshwaran	Gangadharan	Gangulee	Ganguly
Gargeya	Garikapaty	Garlanka	Gavarasana	Gavaskar	Ghandi
Ghani	Ghazali	Ghei	Ghemawat	Ghorpade	Ghosal
Ghosh	Ghoshal	Ghoshdashtidar	Ghouse	Gidh	Gilab
Giridhar	Giridhara	Giridharan	Girish	Girsh	Godambe
Goel	Goenka	Gokaraju	Goli	Gopalakrishnan	Gopalan
Gopinath	Gopivallabha	Gorantla	Gorawala	Gordha	Gorti
Govindasvamy	Govindraj	Gowd	Gowda	Gowravaram	Gridharan
Guha	Gujral	Gundamaraju	Gundlapalli	Gundugollu	Gunendran
Guneratne	Gungabissoon	Guntupalli	Guntur	Gunturu	Gupte
Guramurthy	Gurbux	Gurijala	Gurinder	Gurudutt	Gutala
Gutta	Halder	Hamada	Hament	Harbir	Harishandra
Harku	Haryadi	Hattangady	Hazare	Hemalatha	Himani
Himanshu	Hindocha	Hinduja	Hiranandani	Hiten	Hitendra
Honnenahalli	Huggahalli	Huggahilli	Hynala	Ilango	Ilyas
Imani	Indrani	Innuganti	Irani	Iyer	Jadeja
Jafferbhoy	Jaffrey	Jagarlamudi	Jagder	Jahnavi	Jai
Jaishree	Jaisimha	Jana	Jandhyala	Janjua	Jannavi
Jasmit	Jaspal	Jaspreet	Jasthi	Jayakar	Jayantilal
Jayaram	Jayasinghe	Jayasurya	Jeeri	Jeevan	Jeoomal
Jeyaseelan	Jignesh	Jimuta	Jindal	Jinen	Jinturkar
Jitesh	Joardar	Jobanputra	Jonnalagadda	Joshi	Joshipura
Junanker	Jyothsna	Jyotiradha	Kaalki	Kabir	Kabra
Kachwaha	Kadak	Kadamuddi	Kadowala	Kaikini	Kaisth
Kaith	Kakde	Kalanadhabhatla	Kalirai	Kallakuri	Kallianpur
Kallichuran	Kalluri	Kalpak	Kalpna	Kambhampat	Kambhampati
Kambhatla	Kambli	Kampan	Kandadai	Kandathil	Kandula
Kanetkar	Kanitkar	Kanive	Kankipati	Kanmani	Kansal
Kanwar	Kapadia	Kapoor	Karapiet	Karim	Karkada
Karumuri	Karuppia	Kasavaraju	Kasthurirangan	Kasturirangan	Kateel
Kathiravan	Kathrada	Katka	Katragadda	Kaul	Kaushal
Kaushik	Kawediya	Kayeeda	Kedar	Kedarnath	Kedia
Keerthana	Keerthi	Kelaka	Kenchammana	Keshab	Keshav
Keshava	Keshavan	Keshaw	Kesiraju	Keskar	Ketaki
Keyush	Khadri	Khanderia	Kharbanda	Khilnani	Khodabhai
Khodaiji	Khot	Khursh	Kirmani	Kishen	Kishore
Kittur	Kitu	Kity	Kodali	Kodanda	Kodandarami
Kodi	Kodumudi	Koduri	Koganti	Kola	Kolagunta
Kolala	Kolar	Kommana	Konchady	Konda	Kondapalli
Kondapaneni	Konduru	Koneru	Konkar	Konkipudi	Koothrappally
Koppala	Koppale	Koppolu	Koppula	Koritala	Kosanam
Kosuri	Kota	Kothandaraman	Kothari	Kotla	Koushika
Kripa	Krishnamma	Krishnamurthy	Krithivas	Kriti	Kudesia
Kulasekaran	Kulkarni	Kumar	Kumawagra	Kumbla	Kumble
Kumur	Kunal	Kunderan	Kuntal	Kunwarjit	Kuram
Kurapati	Kurian	Kurinji	Kurtha	Kurupath	Kuruvilla
Kusagra	Kuttikkad	Kutty	Kutumbaka	Labhsha	Laddha
Lahan	Lahiri	Lakhani	Lalima	Lalit	Lalita
Lalitesh	Lalith	Lalitha	Lalji	Lanka	Lata
Latesh	Lath	Latha	Laul	Lavanis	Lavanya
Laxmanan	Lecamwasam	Lokesh	Lokhande	Lolaksi	Lolla
Loy	Luthra	Luthria	Macharla	Mackherdhuj	Macwan
Madan	Maddukuri	Madduri	Madhabi	Madhana	Madhani
Madugula	Magesh	Mageshkumar	Mahabala	Mahadeo	Mahadevan
Mahajan	Mahale	Mahalingam	Mahankali	Mahanthapa	Mahapatra
Mahatapa	Mahatma	Mahavira	Mainak	Maiti	Maitreya
Maitryi	Majety	Majhi	Maji	Majoo	Makam
Makarand	Makhija	Malavika	Malhotra	Malik	Malini
Malipatlolla	Malipeddi	Malleshi	Mallick	Mallika	Mallikarjun
Mallya	Malti	Mamgain	Mamta	Manandhar	Manas
Manasa	Manasi	Manavendra	Manavi	Manchanda	Manchapora
Mandar	Mandava	Mandayam	Mandhatri	Mandyam	Maneesh
Manekshaw	Manesh	Mangalampally	Mangalvedhe	Mangalwadi	Mangesh
Mangeshkar	Mangina	Manglorkar	Manikkalingam	Maninder	Manivanan
Manjanatha	Manjari	Manjrekar	Manju	Manjunath	Manjusha
Mankad	Manmeet	Mannem	Manohar	Manohari	Mansey
Mantri	Manushi	Manyam	Maqbool	Maran	Margasahayam
Marisa	Marita	Markandeya	Markendaya	Maruthi	Maruti
Masrani	Matanga	Matangi	Mathrubootham	Mati	Matta
Matu	Maudgalya	Mavalvala	Maya	Mayappan	Mayekar
Mayur	Mayuri	Mecca	Medapati	Medha	Medikonda
Meenakshi	Meenakshisundaram	Meenan	Megana	Meghana	Mehendale
Meher	Meherhomji	Mehra	Mehta	Mehul	Meka
Melliyal	Merchant	Merchia	Meyappan	Mhambrey	Mhari
Michandani	Mihir	Milan	Milind	Minakshi	Mirajkar
Mirchandani	Mista	Mitali	Mitanu	Mitra	Mittal
Mittur	Mitul	Mitun	Modi	Mohaiemen	Mohanty
Moidu	Mokate	Mona	Mondem	Mongia	Moni
Monica	Mooljee	Moorthy	More	Motala	Motiwala
Mounil	Mousumi	Mrudaya	Muddiah	Mudhol	Mudigonda
Mukherjee	Mukhi	Mukku	Muktheswara	Mukti	Mukul
Mukund	Mukunda	Mukundagiri	Mukundan	Mulla	Multani
Munish	Muniyappa	Munusamy	Muppala	Muqtedar	Murli
Murthy	Murti	Murtugudde	Murty	Murugan	Murugappa
Murugesan	Musunur	Muthanna	Muthiah	Muthu	Muthukaruppan
Muthukrishn	Muthukrishnan	Muthukumar	Muthukumarasamy	Muthupalaniappan	Muthuswami
Naagesh	Nabendu	Nachik	Nachiketa	Nadhamuni	Nadkarni
Naeem	Nagabhushana	Nagalingam	Naganathan	Nagappa	Nagaraj
Nagaraja	Nagarajan	Nagarjuna	Nagaswamy	Nagedwaran	Nagesh
Nageshwar	Nageshwara	Nageswar	Nagi	Nagin	Nagpal
Nahid	Naidoo	Naik	Nailadi	Naimesh	Naimish
Naini	Nakul	Nalini	Nallamothu	Namasri	Namdev
Namrata	Nandakishore	Nandakumar	Nandedkar	Nandin	Nandini
Nandita	Nandkeolyar	Nandy	Nanga	Naoomal	Nara
Naran	Narang	Narasimban	Narasimha	Narasimhan	Narasinha
Narayan	Narayanaswamy	Narayanswami	Narmada	Narsi	Nartana
Naseer	Nashier	Nasir	Natasha	Nath	Nathan
Natterraja	Naueshwara	Navarathna	Navya	Nayak	Nayar
Nayna	Nayudu	Nayyar	Neeharika	Neel	Neela
Neelakantachar	Neelam	Neelesh	Neena	Neeraj	Neerja
Neeru	Neha	Nehru	Neil	Nelagadde	Nema
Nergis	Nerurkar	Nidheesh	Nidhi	Nidra	Nigam
Nihar	Niharika	Nikesh	Nikhil	Niki	Nikitha
Nikunj	Nilani	Nilesh	Nilima	Nilini	Nilofer
Nilu	Nilufar	Nimbalkar	Nimesh	Nira	Niradhara
Niraj	Niral	Niramitra	Nirav	Nirguna	Nirmal
Nirmala	Nirupa	Nirupama	Nisha	Nishar	Nisheeth
Nishit	Nishita	Nishtha	Nita	Niten	Nitesh
Nitesha	Nithin	Niti	Nitin	Nitu	Nitya-Sundara
Niveda	Nivedita	Nizami	Nuguru	Nukala	Nuregesan
Ogale	Omarjeet	Omesh	Omkar	Oruganti	Padmakant
Padmanabh	Padmanabhan	Padmasola	Padmesh	Pahad	Pahwa
Pai	Pal	Palam	Palanirajan	Palanisamy	Palathingal
Palia	Pallavan	Pallavi	Paloma	Palomi	Pals
Palshikar	Pamela	Pancholi	Pandian	Pandit	Pandya
Panick	Panjwani	Pankaj	Pankajakshan	Pankharia	Pant
Panth	Panyala	Paola	Papa	Papatranal	Parag
Paramartha	Parameswaran	Parantap	Paritosh	Parmar	Parnika
Parnita	Partha	Parthak	Parthasarathi	Parthasarathy	Parthathy
Parthiban	Parul	Parvin	Pasapuleti	Pashupathy	Pasram
Pasuma	Patachli	Patanjali	Patankar	Patel	Pattabhiraman
Patterjee	Pauravi	Pavan	Pavanaja	Pavani	Paveljit
Pavi	Pavithran	Pawan	Payal	Pedapudi	Pendharkar
Pendyala	Pennathur	Persaud	Perumal	Perumbeti	Pewar
Phadkar	Phadnis	Phalgun	Phani	Phutika	Pichai
Pillalamarri	Pillay	Pivari	Piyush	Poduri	Podury
Polamreddy	Polavarapu	Ponamgi	Ponnada	Ponnekanti	Ponte
Pooja	Poola	Pothireddy	Potla	Potluri	Prabhat
Prabhath	Prachi	Pradhan	Pradip	Prafull	Pragalsingh
Praharaj	Prajapati	Prajna	Pramath	Pramila	Pramsu
Prasad	Prasai	Prasana	Prasanna	Prasannakumar	Prasanta
Prasanth	Prasata	Prasenjit	Prashant	Prashanth	Prashun
Prasoon	Prassana	Prateek	Pratima	Pratyush	Praveen
Praveenkumar	Pravil	Prayag	Preeti	Preetinder	Preetish
Premkumar	Prerana	Primal	Prisha	Pritha	Prithu
Prithvi	Prithviraj	Priti	Pritish	Privrata	Priyabroto
Priyadarshi	Priyadarshini	Priyanka	Priyavardhan	Priyodarshi	Probal
Profulla	Progyan	Promod	Prudvi	Puja	Pujar
Pulavarti	Puli	Puliyur	Pulkit	Pullela	Pummy
Punati	Pundari	Pundarik	Punita	Punith	Punitha
Punj	Punnoose	Purandhri	Puranjay	Puri	Purujit
Purva	Pusan	Pushkala	Pushkar	Pushkarini	Puskara
Pusti	Pyara	Qamar	Rabinder	Rabindra	Rabindran
Rachna	Rachoor	Radhabinod	Radheshyam	Radhey	Radia
Ragha	Raghavanpillai	Raghavendra	Raghavendran	Raghunandan	Raghunathan
Raghuram	Raghuvir	Raghvendra	Ragunathan	Raguraman	Rai
Rajabhushan	Rajagopal	Rajagopalan	Rajah	Rajal	Rajamani
Rajan	Rajani	Rajaram	Rajarama	Rajaraman	Rajarshi
Rajashi	Rajasimha	Rajat	Raje	Raji	Raju
Rakala	Rakhi	Ramadhin	Ramadin	Ramakan	Ramalingam
Ramamani	Ramamohan	Ramamoorthy	Ramamurthy	Ramamurti	Ramamuthe
Raman	Ramana	Ramanakoppa	Ramanand	Ramanathan	Ramanuja
Ramanujam	Ramaprasad	Ramasamy	Ramasubraman	Ramasubramanian	Ramaswami
Ramaswamy	Ramchand	Ramchander	Ramchandra	Ramdas	Ramiah
Ramila	Ramjee	Ramji	Ramkumar	Rammohan	Ramnarine
Ramprakash	Ramprakesh	Ramprasad	Ramsamooj	Ramsundar	Rana
Ranadhir	Ranadive	Randeep	Ranga	Ranganathan	Rangaraj
Rangarajan	Rangarathnam	Rangaswamy	Rangnekar	Rangwala	Ranhotra
Ranjan	Ranjana	Ranjani	Ranjini	Ranjitsinhji	Rantidev
Rasiah	Rathiea	Rathin	Rathore	Ratnasabapathi	Ravandur
Raven	Ravikanth	Ravikumar	Ravipati	Raviprakash	Raviraj
Raviram	Ravuri	Raychaudhari	Raza	Reba	Rebani
Reema	Rege	Rekha	Rema	Rengarajan	Renuka
Renukunta	Resham	Reshma	Revathi	Revati	Rewari
Richa	Riddhi	Riju	Rima	Rishi	Rishiyur
Rishmal	Ritula	Rob	Robi	Roopak	Roshni
Roy	Ruchi	Ruchika	Ruchir	Ruchira	Rudrani
Rudraraju	Rukmini	Ruma	Rupa	Rupali	Rupesh
Rustagi	Saandeep	Sabeena	Sabeer	Sachi	Sachin
Sadalge	Sadaram	Sadashiv	Sadasivam	Sadayappan	Sadhwani
Saeed	Sagar	Sagdo	Saginala	Sagoo	Sahadev
Sahar	Sahgal	Sahil	Sahni	Sai	Saibal
Saidullah	Saighiridhar	Saikumar	Sailendra	Sailesh	Saini
Sajal	Sajan	Sajeev	Saji	Sajid	Sajja
Sakib	Saklani	Salagame	Saldanha	Saligrama	Salil
Salim	Saloni	Saluja	Sambandam	Sambandan	Samderiya
Sameer	Samiksha	Samir	Samit	Sammeta	Sampath
Samrat	Samudra	Samuel	Sanaka	Sanat	Sandeep
Sandip	Sandipa	Sandipan	Sandy	Sangal	Sangam
Sangameswar	Sangappa	Sangawar	Sangeeta	Sangem	Sangha
Sanghi	Sangita	Sangodkar	Sanigepalli	Sanjna	Sanjukta
Sankait	Sankaran	Sankaranarayanan	Sankrant	Sankuratri	Sanmugasunderam
Santanu	Santhanakrishnan	Santhanam	Santharam	Sanu	Sanyogita
Sanyukta	Sanzgiri	Sapan	Sapna	Sapra	Sapthotharan
Sara	Saraf	Sarangapani	Sarangarajan	Sarasvan	Sarasvati
Saraswathi	Sarat	Sarath	Saravati	Sardesai	Sarika
Sarin	Saripella	Sarita	Sarmad	Sarmistha	Saru
Sarup	Sarwate	Saryu	Sashekala	Sashi	Sashti
Sasthi	Saswata	Satayu	Sathasivam	Sathaye	Sathiamoorthy
Sathianarayan	Sathyanarayana	Sathyanna	Satin	Satinder	Satrujit
Satsangi	Sattar	Satwant	Satyanarayana	Satyavati	Satyavolu
Satyavrat	Satyen	Saumya	Saunak	Saurin	Savarna
Savdeep	Sawalha	Sawant	Sawardekar	Saxena	Scindia
Seetamraju	Seetharaman	Sehgal	Sekar	Sekariapuram	Sekhar
Selvam	Selvi	Semerkant	Sen	Senagala	Senajit
Senapathy	Senapati	Sengupta	Seri	Seshaanath	Seshadri
Seshadrinathan	Seshan	Seshu	Sethi	Sethuraman	Setna
Setra	Shaban	Shabi	Shachi	Shadilya	Shafiqul
Shahbaz	Shahid	Shaik	Shail	Shaila	Shailaja
Shailendra	Shailesh	Shaina	Shaje	Shalabh	Shalaby
Shally	Shameem	Shamir	Shamshaard	Shamsher	Shan
Shanbhag	Shantinath	Sharda	Shareeka	Sharma	Sharmistha
Shashank	Shashidhar	Shashikanth	Shashwat	Shastri	Shaukat
Sheba	Shefali	Shetty	Shibu	Shikha	Shiladitya
Shilpa	Shindi	Shingane	Shinjinee	Shinu	Shirish
Shirishkumar	Shirvaikar	Shishir	Shivaiah	Shivakumar	Shivani
Shivaprakash	Shokrollahi	Shomik	Shorey	Shourov	Shraddha
Shradhdha	Shreekant	Shreenath	Shreerang	Shreeyash	Shrestha
Shridhar	Shriharsha	Shrikant	Shrinivas	Shripati	Shrirang
Shrisha	Shrivastava	Shroff	Shruti	Shubha	Shubhabrata
Shubhashish	Shubhendu	Shujauddin	Shukla	Shukta	Shurpali
Shvetank	Shyamsundar	Sibabrata	Sidda	Siddhi	Sidhu
Simha	Sinduvalli	Singri	Sira	Sita	Sitha
Sitipala	Sivakumar	Sivakumaran	Sivaram	Sivaramakrishnan	Sivaraman
Sivasubramaniam	Sivasubramanian	Smirti	Smita	Smitha	Snehasis
Snigdha	Sobha	Sodhani	Sohal	Sohoni	Solaimathi
Solanki	Solkar	Soma	Somasundara	Somasundaram	Somatra
Somayaji	Somendra	Somnath	Sompalli	Somu	Soni
Sonia	Sony	Soogoor	Sophia	Sorabhjee	Sornam
Soumen	Soumitra	Soumodip	Soumyabrata	Soundar	Soundrapandian
Sourabh	Sourajyoti	Sourav	Sowrirajan	Sraddha	Sravan
Sree	Sreedevan	Sreedhar	Sreedharan	Sreehari	Sreekanth
Sreekanthan	Sreenivas	Sreenivasa	Sreenivasan	Sreeram	Sreerupa
Sreevijayan	Sridevan	Srihari	Srijata	Srijoy	Srikaran
Srikrishna	Srikrisna	Srikumar	Srila	Srimal	Srinath
Srini	Sripadam	Sriramesh	Sritharan	Srivas	Srivastav
Srivastava	Srivaths	Srivathsan	Srivatsan	Sruthi	Sruti
Sthanumurthy	Subas	Subba	Subbanna	Subbarao	Subbarat
Subbaratnam	Subbarayan	Subbarayudu	Subhaga	Subhangi	Subhendu
Subhuja	Subodh	Subrahmanyam	Subram	Subramani	Subramaniam
Subramanian	Subramanien	Subramanium	Subramanya	Subramanyan	Subrata
Subudhi	Sucharita	Suchi	Suchin	Suchitra	Sudarsan
Sudarshan	Sudarshana	Sudershan	Sudesh	Sudesha	Sudeshna
Sudeva	Sudevi	Sudha	Sudhakar	Sudhanshu	Sudhansu
Sudheer	Sudhindranath	Sughavanam	Sugriva	Suhas	Sujan
Sujeet	Sujeev	Suji	Sukanya	Sukarman	Suketu
Sukhjinder	Suksma	Sukumar	Sulagna	Sultana	Suman
Sumanna	Sumedh	Sumeet	Sumila	Sumon	Sunanda
Sundaramoorthy	Sundararajan	Sundhararajan	Suneina	Sunny	Sunondo
Sunrita	Sunthari	Sunther	Suppiah	Supriya	Suprotik
Surabhi	Suranjan	Surapanani	Surapaneni	Surati	Suravinda
Surekha	Surendar	Surendra	Surendran	Surendranath	Suri
Suriyaprakash	Surnilla	Surotama	Surpur	Surti	Suruchi
Surujnarine	Surupa	Sury	Suryadevara	Suryanarayama	Suryanarayan
Suryanarayana	Suryanarayanan	Susan	Susarla	Susumna	Sutapa
Suthar	Suvrata	Swagat	Swagato	Swami	Swaminathan
Swamy	Swani	Swanimathan	Swarnkar	Swathi	Swati
Sweta	Swetha	Syamala	Tagore	Taksa	Talip
Talwar	Tamhane	Tammana	Tamragouri	Tandekar	Tanmaya
Tantry	Tanu	Tanuj	Tanuja	Tapan	Tapas
Tapesh	Tapi	Tapti	Tarang	Tarit	Tarpa
Tarpana	Tasha	Tasneem	Tatat	Tatavarti	Tendulkar
Tetegni	Thadigiri	Thakarta	Thakur	Thamma	Thamry
Thandray	Thangaraj	Thangavadivelu	Thiagarajan	Thimanniya	Thirumalai
Thirumalaiswamy	Thirunarayan	Thirunavu	Thiruvengadathan	Thogulva	Thommana
Thribhuvana	Thukral	Thulasidas	Thundayal	Thundyil	Thuraisingham
Thuvaradran	Thyagarajan	Thyagaraju	Tickoo	Tikaram	Tikekar
Tikku	Tikoo	Tilak	Tina	Tirumalai	Tirumalesa
Toodi	Trikha	Trilochana	Trisanu	Trishna	Trishwant
Trupti	Trusha	Tuhin	Tuhina	Tumkur	Tummala
Tupil	Tushar	Tusti	Tuteja	Tyagi	Tyagri
Ubriani	Uday	Udaya	Udayan	Uddin	Udipi
Udit	Udutha	Ujjaval	Ujjwal	Ujjwala	Ujwal
Ulind	Ulla	Ullas	Umakanta	Umesh	Umrigar
Unmesh	Unnikrishnan	Upanishad	Upender	Upendra	Uppalapati
Uppuluri	Urimindi	Urjavaha	Utpal	Utpat	Uttanka
Uttara	Vadakke	Vadivelu	Vadlamani	Vaibhav	Vaidheeswarran
Vaidhyanathan	Vaidya	Vaikuntam	Vairaja	Vaisakhi	Vaish
Vaishnavi	Vaithu	Vajipeyajula	Vajpayee	Vajpeyi	Vakil
Vallath	Valli	Vallurupa	Vallurupalli	Valsan	Vamshi
Vamsi	Vanchinathan	Vandana	Vandita	Vani	Vaninadh
Vaninadha	Vanita	Varad	Varadarajan	Varahabhotla	Varati
Varganti	Varghese	Variya	Varki	Varsha	Varun
Varuni	Vasava	Vasavi	Vashisth	Vasi	Vasudev
Vasudha	Vasudhara	Vasuki	Vasuman	Vasumati	Vattikota
Vattyam	Vavveti	Vedananda	Vedanga	Vedati	Vedavyasa
Vedula	Veena	Veer	Veera	Veeramany	Veeraraju
Veerasamy	Veerender	Velaga	Vellanki	Vellore	Velusamy
Vemireddy	Vemuganti	Venkatadri	Venkataraghavan	Venkataraman	Venkataramanan
Venkatasubramani	Venkatasubramanian	Venkatesan	Venkatesann	Venkateshwara	Venkateswaran
Venkateswarn	Venkatraman	Venktesh	Venu	Venugopal	Venugopalan
Verma	Vibha	Vibhuti	Vichur	Vidi	Vidur
Vidvan	Vidwans	Vidya	Vidyarthi	Vidyasagar	Vidyashankar
Vijaya	Vijayabhas	Vijayagopalan	Vijayakumar	Vijayalakshmi	Vijayanath
Vijayarangan	Vijayashree	Vijaykrishna	Vijaykumar	Vijaysaradhi	Vikriti
Vikul	Vilok	Vinata	Vineet	Vineeta	Vinit
Vinita	Vinuta	Vinutha	Vipin	Vipperla	Vipul
Viraf	Viraj	Viral	Virani	Virasana	Virat
Virendra	Viresh	Virinchi	Virini	Virmani	Visala
Visalakshi	Vish	Vishaal	Vishal	Vishnavi	Vishnuraman
Vishwa	Vishwamber	Visvajit	Visvakarman	Visvanathan	Visvayu
Viswanath	Viswanathan	Viswesh	Visweswaramurthy	Vittal	Vivatma
Vivek	Vivekanand	Vonguru	Vootla	Vraman	Vuppula
Vyapari	Vyshali	Wadekar	Waman	Yadavalli	Yadgiri
Yaksha	Yalamanchi	Yalamanchili	Yalamanchilli	Yamini	Yamni
Yamura	Yanamandra	Yarlagadda	Yashodhar	Yashodhara	Yashovarman
Yateen	Yauvani	Yavar	Yavatkar	Yegammai	Yellepeddy
Yelsangikar	Yeluri	Yerramilli	Yesh	Yeshonath	Yogendra
Yogesh	Yogish	Yudhajit	Zahin	Zev"""

let cmale = """Abban, Abb« n, Ab« n	 	'little abbot'
€ bhartach
Adomn« n, Adhamh« n, Adhamhnán, Adamnan, Eunan	 	'timorous'
Adanodan
Ailbhe, Ailbe, Alby	 	albho, 'white'?
Ailgel, Ailill
Ailín
Aininn
Ainmire
Airechtach
Airmedach
Alabhaois, Alaios
Alastrann
Alchad
Alstrom
Amalgaid
Amergin
Anluan, Anlon	 	'hound/warrior?
Aodh, Aodha, Aoidh, € ed, € edh, Aedus	Aodhaigh, Aodhán, Aodán, , Áedán, Aiden, Aidan, Edan, Aodhagán, € educ« n, Aogán, Egan	'fire'
Aodhfin
Aonghas, € engus, Aonas, œ engus, Angus	 	'one choice', 'sole strength'
Ardar
Árdghal, Ardal, Argal, Artegal, Arthgallo	 	ard 'high' or art 'bear' + gal 'valour'
Art	Artan	'bear'
Artúr, Artuir	 	Arthur
Baeth	Baetan
Balor
Bairne
Baithaus
Banan
Banbhan
Baothghalach
Barrfind, Bairrfhionn, Barrin	Bairre, Barre, Barra, Barry	white-headed
Beacán
Beag
Beairtle
Beanón
Bearach, Berach	 	Pointed/sharp
Bearchán
Beartlaí
Becan
Behellagh, Behillagh
Benen
Beolagh
Beothach
Bercan, Berchan, Bercnan
Bergin
Blanaid
Boethis
Bran
Brandubh, Brandrub, Branduff
Breadán
Bréanainn, Breandán, Br¾ nainn, Brendan	 	'brand/flaming sword' or Welsh breenhin 'prince'?
Bresal, Bressal, Brasil, Breas, Breasal
Brian, Brion	 	high/noble
Bricc
Britanmael
BrÙ g« n, BrÙ cc« n
Brón
Bruaidheadh
Bruatur
Bruddai
Buadhach
Buagh
Buaigre
Cadhla
Caeilte
Caentigern, Kentigern
Cainchinne?
Cainnech, Cainneach, Canice	 	handsome, fair one
Cairbre, Coirbre, Cairpre
Caircil, Cearcill
Caireall, Cairell, Coireall, Kerill
Cairthinn
Caiside
Calbhach, Calvagh, Calbach	 	'bald'
Callough
Caoilte
Caoimhe	Caiomhín, Caoimhín Caiomheán, Caomhgain, Cáemgen, Coemgen, Kevin, Kevan	caomh 'kind/gentle, beloved/beautiful'
Caolán, Caolainn? Kelan
Caraid
Carantoc
Cárthach, Cartach, Cartagh, Carthagh
Cascorach
Cassidan
Cathal, Cahal, Cathald, Catheld, Cathaldus, Kathel	 	'battle + strong'
Cathan, Cah« n, Cahan, Kane	 	'battle + rule'
Cathaoir, CathaÍ r, Cahir, Cathfer, Caffar	 	'cath' battle + 'vir' man
Catharnach, Cathrannach?
Cathasach
Cathbadh, Cathbad
Cathbharr
Ceallach, Ceallagh, Ceallachán, Ceollach, Cellach, Kellach, Kelly	 	ceall 'monastery/church' or 'bright-headed'
Cearbhall, Cerbhall, Cerball, Cearul, Cearull, Carroll	Cearbhallán	'cearbh', hacking?
Cearnach
Cedach
Celsus
Celtchair
Cenn
Cesarn
Chattan
Chulain
Cian, Kian, Kean, Keane	Cianán	'ancient/enduring'
Ciabh« n
Ciárán, Kieran, Queran?	 	'ciar' black
Ciardan
Ciardha
Ciardubhán
Ciarrai
Cillian, Cillín, Cill¾ ne, Killian, Kealan, Kilian, Killan	 	'ceallach' strife or 'ceall' church
Cinneíde, Cinneídigh	Cinneíddin
Cionadh, Cionaodh, Cin« ed, Cin« eth, Cin« edh, Kenneth
Cnán
Coan
Cobhran
Cognat
Colcu
Comhghall, Comhgall, Comgal, Comgell, Congal, Cowal	 	'together + pledge/hostage'
Comhghán, Comghán, Comgán, Comman? Congan	 	'together born' ie twin
Conaing
Conall, Connell	 	'wolf + strong'
Con« n, Conan, Conant	 	'cu' hound
Conchobar, Conchobhar, Conchubhar, Conchubor, Conchúr, Cnochúr, Conquhare, Conaire?, Connor, Conor	 	'lover of hounds/wolves?'
Congalach
Conganchas
Conn	 	'chief'
Connlaeth, Connlaoth, Connlaodh, Connlaogh, Connlaoi, Conleat, Conl« ed, Conleth, Conla, Conley	 	'chief' + 'lord' (conn + flaith)
or 'prudent/chaste' + 'fire' (connla + « edh)
Coplait
Cormac, Cormacc, Corb
corbaid 'defile' + mac 'son'
Cothric
Couleth
Cridan
Crimhthann
Crofinn
Cromanus
Crónán, Cronin	 	swarthy'
Crosson
Cruamthain
Cuán	 	little hound
Cuileann, Cuileán, Cuilén, Cuilinn, Cullen	Cuilenn« n?
Cuimín, Comyn	 	'cam' twisted
Cuinn
Cuirithir
Cumall
Cúmhaighe, Cúmhaí
Curoi
Curran
Cuthacar
Daig, Daigh	 	'flame/fire'
DaimÍ ne, DaimhÍ n, DaimÍ n, Davin	 	dam 'deer/ox'
Dálach, Daly, Daley	 	dál 'assembly'
Damaen, Daman, Dámnh
Dara, Darach, Daragh, Darragh, Darrah, D« ire, Daire	 	'oak' or 'fertile'
Déaglán, Decl« n, Declan
Deicola
Dela
Demna
Desle
Desmond, Desmumnach, Deasún	Dessi	'man of Muman'; ie from South Munster
Devlin
Diarmait, Duirmhuid, Diamit, Diarmaid, Diarmuit, Dermot, Kermit	Darby	'without envy?'
Dieul
Dimnaus
Disisbod
Diuma, Dimma
Doibhilin
Doireidh
Domhnall, Domnall, Donál, Dónall, Dónall	 	'world + mighty'
Donn, Don	 	brown/king
Donnabhán
Donnán, Donan
Donnchadh, Donnchad, Donncha, Dunchad, Donagh, Donogh, Donough, Dunecan	 	'brown' + 'battle or lord'
Donndubhán, Donndubán, Donovan	 	donn' brown + 'dubh' black
Donngal
Doran	 	'desc of Deoradhán' exile/wanderer
Dorchaidh
Dubhaltach, Dubaltach, Dubultach, Dualtach	Dualta, Duald (Dudley)	'black-haired/dark-limbed'
Dubhán, Duban Duane, Dowan?	 	'dark/black'
Dubhdara, Dubhdarach	 	'black' + 'of oak'
Dubheidir
Dubghall, Dubgall, Dougal	 	'dark foreigner' (ie a Dane)
Dubhghlas, Dubhglas, Dughlas, Douglas	 	'dark' + 'blue'
Duigenan
Dungal« ch
Eachann	Ea
Eachdhonn
Éadbhard
Éanna, Énnae, Enda	 	'birdlike'?
Earna	Earnán
Echen
Éibhear, Éibhir, Heber
Éigneach, Aneas, Aeneas	Éigneachán	violent fate, death
Eimar
Eiméid
Éimhín, Éimíne, Evin, Evan	 	¾ im 'ready/prompt'?
EirnÍ n, ErnÍ ne, Ernin, Ernan	 	iarn 'iron'?
Eithear
Elochad
Emianus
Ennae
Eochaidh, Echaid
Eogabail
Eóghan, Eogan	 	'born of the yew'
Eolus
Erc, Ercus
Eremon
Fachnan
Fachtna, Fiachna	 	'hostile'?
Faebhar
Fáilbe, Fáilbhe
Failge
Faolán, F« el« n, Felan, Foelan, Fillan	 	'wolf'
Farann« n
Faughnan
Feagh
Fearadhach, Fearadagh, Farry	 	'manly'
Feardorcha, Ferdorcha, Fardoragh, Ferdorccha	 	Man + dorcha 'dark'
Fearghal, Fearghall, Fergal, Ferghil, Ferol	 	fear 'man' + 'gal' valour
Fearghas, Fearghus, Fergus	 	'man' + gus 'vigour'
Feichín, FechÍ n, FechÍ ne, Fehin, Fechin	 	'fiach' raven
Feidhlim, Feidhlimidh, Fedelmid, Feidlim, Féilim, Felim, Felimy, Phelim	 	'ever good'?
Feuillan, Fillan
Fiachra, Ficare, Fiach	 	fiach 'raven'; or 'battle king'
Find	Findan
Finegas
Fingar
Fínghín, Finghin, Fingin, Fínín, Fineen	 	wine + born
Finn, Fionn	Fionnán, Finnian, Finian, Finnén, Fionán, Fianan	white/fair
FÍ nnachta
Finnchad	 	White + battle
Finntan, Fionntán, Fintan, Fiontan, Fiontán	 	'white/fair' + 'ancient' or 'fire'
Fionnbhárr, Fionnbhar, Findbarr, Finbar	Bairre, Barry, Barra	'white head'
Fios
Flaithbertach
Flaithrí, Flurry, Florry	 	'flaith' prince/leader + 'rí' king
Flann, Flainn	Flannán, Flannacán (Florence)	'red/ruddy'
Flannchadh	 	'red warrior'?
Fochmare
Fogartach
Foillas
Forgael, Forgall
Fortchern
Frainc
Froichan
Fuatach?
Fulan, Firlan
Fursa, Fursey
Gaithan
Gall, Goll
Gallech
Garbhán, Garb« n, Garvan	 	garb 'rough'
Garfhidh
Garnard, Garnat
Gilian
Glaisne
Glassan
Gnathach
Gobb« n, Gob« n, Gobann, Goban	 	goba 'smith'
Gordan
Gorm« n, Gorman	 	'dark/swarthy'
Gosan, Gusan
Gráda
Guaire	 	'noble/proud'
Herygh
Hewney
Huydhran
Iarbonel
Iarlaith, Iarlaithe, Iarfhlaith, Jarlath	 	'ior' ? + 'flaith' leader
Iobhar,  bhar, Ibhor, Ibor, Abaris	 	iobar 'yew tree'
Ighneachán
Imchath
Incha
Indract
Ingnathach
Ióéil
Íosóg
Irial
Irimia
Iucharba
Iúd
Iúil
Joavan
Kenncoh
Kescog
Labhcás
Labraid, Labhraidh, Lowry	 	'speaker'
Labr« s, Labhr« s Libra? (Laurence)	 	'laurel bush'
Lachtna	 	'milk-coloured'
Laegh, Leagh, Laoghaire, L« egaire, Laeghaire, Leary	 	'calf-herd'
Laistranus
Laoighseach, Laoiseach	 	from Laois, Leinster
Laisr¾ an, Laisr¾ n	Molaisse, Molaise	lasair/laisre 'flame' + 'fíon' wine
Lavren
Leann« n, Lenn« n	 	'sweetheart/lover'
Lithgean
Lochlainn, Lochlann, Leachlainn, Laughlin, Loghlin	 	from Norway, land of the lochs?
Lodan
Lomán, Lomm« n	 	lomm 'bare'
Lonán	 	lon blackbird
Lorcán, Lorcann, Lorccán	 	lorcc 'fierce/dumb/cruel'?
Lua
Luchta
Lugh, Lughaidh, Lugaidh, Lughaid, Lugaid, Luger, Lewy	 	bright/shining
Lysagh
Machar
Maduta
Maedoc, Maidhc, MaodhÙ g
Mairid
Manchan
Manus
Mathghamhain, Mathúin, Mathghamhaim, Mathgamain, Mahon	 	bear
Meadhran, Medran
Meallán, Mell« n, Mellan, Mullin?	 	'lightning'
Medabh
Mel
Meldan
Melkorka
Melrone
Meubred
Midir, Midhir, Mider?
Mirin
Mo-Bioc
Mochoemoc
Mochta, Mochteus, Mochua
Mochumma
Modomnoc
Mogue
Molling
Moloi, Molua, Moluag
Mong« n
More
Morna
Muchin
Mughran
Muirghean
Muirgheas, Muirgius, Muiris	 	'muir' sea + 'gus' choice
Muiriartach, Muicheachtach, Muireadhach, Muirchertach, Muiredach, Muiríoch, Murtagh, Murty, Briartach	Muirí	muiredach 'lord/master' or 'seaman/mariner'
Munnu
Mura, Muranus
Murchad, Murchadh, Murcha, Murrough, Muru	 	'sea-battler'
Naoise, NÙ ise
Nathi
Nemid
Nevan
Niadh
Niall, Neal, Neill, Neil	 	nel 'cloud', or passionate? champion?
Ninian
Notal
Nuallán	 	nuall 'champion'
Odhrán, Odr« n, Odran, œ r« n, Oran, Odhar	 	odhar 'dun/sallow'
Ógán
Oisín, OissÍ ne, Ossian, Oisin, Osheen	 	os 'deer'
Oscar	 	'deer' + cara 'friend'
Otteran
Ounam
Phelan
Piran
Rádhulbh
Raghallach, Reilly, Riley
Riaghan, RÍ ogh« n, RÍ an, RÍ g« n, Rian, Ryan, Royan	 	ri 'king'
Rónán, Ronan, Rownan	 	rón 'seal'
Rórdán, Riordan, Reardan, Rearden, Ríoghbhardán	 	king + poet/bard
Ros
Ró adhán, Ró ad« n, Rhod« n, Rodan, Ruan, Rowan	 	'red-haired'
Ruaidhrí, Ruaidrí, Ruairí, Rudraighe? Rory, Roderick, Roricus	 	'red/great' + 'king'
Ruidhe
Saebhreathach, Sáerbrethach, Saorbhreathach
Saoirse	 	'freedom' (mod.)
Sanctan
Saranus
Scelianus
Scolaidh, Scolaighe
Sé
Seachnall
Séafra, Seafraid
Séaghdha, Ségdae	 	hawklike/fine/godly?
Seanán, Senan, Seanach, Senach	 	sen/sean old/wise
Séarlan
Sedna
S¾ g« n, Seaghán? Segenus
Sheary
Shiel
Siadhal, Siaghal
Siochfioldha
Sinon
Siran
Siseal
Sól
Starn
Steimhin
Suibhne, Suibne, Sweeney, Sivney?	 	'well-going'?
Suthan
Tadhg, Tadg, Tadc, Tegue, Teigue, Teige, Taig, Taidgh, Tiege	Taidhgín	'poet/philosopher'
Tathai
Tiamhdha
Tighearnach, Tighernach, Tiarnach, Tiarna, Tierney	 	tighearna 'lord'
Tighearnán, Tigern« n, Tiarnán, Tiernan	 	''
Tiomóid
TÍ rech« n
Toirdhealbhach, Toirdhealbharch, Tairdelbach, Toirealach, Tárlach, Tirloch, Traolach, Turlough, Turley, (Terence)	 	'instigator/abettor'
Tomaltach?
Torrianus
Treon
Tó athal, Tuathal, Toal	 	ruler of a tribe
Tyrone	 	placename
Uaithne
Uallachán
Uar
Uileos
Uillen
Úistean
Ultán, Ultan	 	'an Ulsterman'
Urthaile
Usliu
Uthmaran
Vigean
Wyllow	 	 """

let cfemale = """Acht« n
Aclitenis
Aibfinna
Aifric, Affrica
Ailbe, Ailbhe	 	albho 'white'?
Ailidh
Áine, Aime, Enya	 	'brightness/splendour
Aisling, Aislinn, Ashling	 	'dream, vision'
Alannah	 	o leanbh - 'oh, child'
Álmaith
Almha, Alva, Almu, Alma
Anastas
Anga
Annábla
Aodhamair, € eddammair	 	'fire' (from masc. Aodh/Aedh)
Aodhnait, Aednat, Enat	Aedin? Aideen?	'fire' (from masc. Aodh/Aedh)
Aoibh, Aiobheann, Aoibhinn, AoibhÍ n, Eavan	 	'beautiful'
Aoife, Aoiffe, Aeife, AÍ fe, Aife	 	'beautiful/radiant'
Arlene
Athracht, Attracta
Aurnia
Barran
Beatha
Becuma
B¾ bhionn, Béibhinn, Bébhinn, B¾ binn, Bebin, B¾ find, Befinn, Bevin, Vevina?	 	'white lady'
Bega
Beirnís
Bél
Belocc
Beonill
Berrach, Bearach	 	'sharp/pointed?'
Berriona
Bidina
Bil
Bláth	Bláithnait, Bláthnaid, Blathnait, Bláthnat, Bláithín, Bl« naid, Blanid	'flower'
Bluinse
Bodhbh
Brenda, Breanda	 	'brand, flaming sword?'
Bríghid, Brídget, Brigid, Brigit	Brede, Breeda, Bríd, BrighdÍ n, Brídín, Bedelia, Bidelia, Biddy, Biddie	'high goddess'
BrÙ nach	 	'sorrowful'
Cacht
CaÍ ntigerna, Kentigerna, Quentigerna
Caoilfhionn, Caoilainn, Caelfind, Coelfinnia, Keelin	 	'slender + fair/white'
Caoimhe
Caral
Cathan, Cah« n, Cahan, Kane	 	'battle + rule'
Ceara, Cera, Cara, Carra	 	'red'?
Cearúilín
Ciannait, Ciannata	 	Fem of Cian
Ciara, Ciar, Cyra, Keara, Kiera	 	'black/dark'
Ciarda
Cíit
Cingit
Clíodhna, Cliodna, Clídna, Clíona, Cliona, Cleena	 	a fairy princess
Clodagh, ClÙ ideach, Cloda	 	river name (or Claudia)
Clothra
Cochrann
Cóemfind
Coímell
Colan
Colleen	 	'girl/wench' (mod.)
Conandil
Conchenn, Coinchind
Conchobarre	 	Fem of Connor/Conchobar
Congan
Creda, Crida
Cróeb
Cron
Cuach
Cumman
D« irÍ ne, Daireen, Darina	 	'fruitful/fertile'
Damhnait, Damhnat, Davnat, Devnat, Dymphna, Dympna	 	damh 'fawn'
Dana, Danu, Ana, Anu	 	'wealth/abundance'
Danann
Dar-Cárthaind
Dareca
Dearbháil, Derbáil, Derbh« il, Deirbhile, Derval, Dervila, Dervla, Devla, Devorguilla, Derb-Forgaill	 	2 names, 'daughter of Fal' (ie Ireland), + 'daughter of the poet'
Decla	 	Fem of Declan
Delbchaem
Dercco
Derdraigen
Derdriu, Deirdre, Dierdre	 	'woman?' 'she who chatters'?
Derg
Devnet
Doireann, Dorean, Dóirín, Doreen, Dorren, Dorinnia, Doirind, Doirend, Dorene, Dorine, D« irinn, Derinn	 	der 'daughter' + Finn?
Donelle
Donnfhlaidh, Donnflaith, Dunflaith, Dunlaith, Dunla, Donla	 	'brown' + 'lady/princess'
Downett
Drón
Éabhna
Ealga
Echna	 	>ech 'steed'
Edana
Éibhliu, Ébliu, Éblenn, Éibhleann, Evle, Evlin	 	Ù iph 'beauty/radiance'?
Eihrig
Éile
Eilgri
Eimear, Eimhear, Eimer, ‹ mer
Éimhín, Éimíne, Evin	 	¾ im 'ready/prompt'?
EirnÍ n, ErnÍ ne, Ernin	 	iarn 'iron'?
Eithne, Ethna, Uaithne? Ethne, Ethenia, Ethnea, Etna, Edna, Ena	 	'kernel' or aitten 'gorse'
Elan
Elige
Elva
Enda	 	bird
Érennach
Eri
Erin, Éirinn	Eriu?	Ireland
Étáin, Éadaoin, ‹ daein, Etain, Aidin	 	et 'jealousy'
EthlÍ nn
Étromma
Faílend
Faimdid
Fainche, Fanchea
Fand, Fann
Faoiltiarna, Whiltierna	 	'faol' wolf + 'tighearna' lord
Feidhelm, Fidelma, Feidelma, Fedelma, Fedelim, Feidlim, Fedelm, Delma	 	'ever good'?
F¾ thnaid, F¾ thnat, Fenit
Find
Findchóem
Findétand
Findscuap
Finnsech, Finnseach, Finsha	 	'fair lady'
Fíona, Fína, Finna, Fionna	 	vine
Finnabhair, Fionnabair, Fionó ir, Fennore	 	'white ghost'?
Fionnghuala, Fionnuala, Finnguala, Fenella, Finella, Finvola, Finola	Nuala, Nola	'white shouldered'
Flann	 	'red/ruddy'
Fodla, Fodhla, Fola
Fuamnach
Garb
Glóir
Gobnait, Gobinet, Gobnat, Gobnata, Gobnet, Gubnet	 	goba 'smith'
Gorm« n, Gorman	 	'dark/swarthy'
Gormflaith, Gormlaith, Gormla, Gormley	 	'splendid or blue' + 'princess'
Gráinne, Grania, Granna, Granya	 	'grain' or 'disgust/terror'
Grian	 	'sun' or 'sun goddess'
Guinnear
Hisolda
Hya, Ia
Ibel
Íde, Ida, Ita, Ite	 	itu 'thirst' or 'eating/devouring'
Indécht
Indiu
Inis
Isleen
Keeley, Keelin
Keenat, Kinnat
Labhaoise
Lára
LasairfhÍ ona, Lasairíona, Lasairian, Laserian, Laisrian, Lasairíona, Lassarina, Lasrina	Lassar, Lassi	lasair/laisre 'flame' + fíon 'wine'
Lebarcham, Leborcham
Lethann
LÍ adan, LÍ adain	 	'grey lady'?
Liban
Life	 	goddess of river Liffey
Lile
Luigsech, Luighseach, Luigseach, Luíseach, Laoise, Laoiseach	 	'radiant girl'
Macha
Máda
Magael
Mallaidh
Méabh, Meadhbh, Maedbh, Maedhbh, Maeve, Meave, Medb, Mave	MeidhbhÍ n	'intoxicating'
Meld
Míde, Meeda
Mincloth
Mise
Móen
Moncha
Monenna, Moninna, Moninne	Blinne, Bline, Ninne
Mongfhionn, Mongfind
Mór	Moreen, Morrin, Móreen, Móirín	'great/tall'
Muadhnait, Muadnat, Muadhnata, Monat, Mona	Muadhnatan	muadh 'noble'
Mugain
Múireann, Muirinn, Murinnia, Muirenn, Murainn, Mairenn, Miren, Maren	 	muir 'sea' + fionn 'white/fair'
Muirecht
Muirgen, MuirÍ n, Muirenn, Miren	 	'born of the sea'
Muirgheal, Muirgel, Muiríol, (Muriel)	 	'sea-bright/white'
Muiriath
Muirne, Myrna, Merna, Morna, Moina, Moyna	 	'beloved'
Naomh	 	'a saint' (mod.)
N« rbflaith, Narvla	 	'noble princess'
Neasa, Neassa, Nessa, Ness
Némdaille
Niamh, Neamh, Niam, Ném? Neave	 	'brightness/beauty'
Nóinín
Noleen, Nolene, Norlene
Óchae
Odharnait, Odarnat, Odhamnait, œ rnat, Ornat, Orna, Ownah	 	odhar 'dun/sallow'
Óebfinn
Ohnicio
Oilbhe
Órfhlaith, Orflath, Órlaith, Orlagh, Órla, Orla	 	or 'gold' + flaith 'lady/princess'
Osmanna
Paili
Piala
Rathnait, Ranait, Rónnait, Ronit	 	'rath' grace/prosperity
Réaltán
Rigan
RÍ oghnach, Roighnach, RÍ ghnach, RÍ onach, Rynagh, Rinach	 	'queenly'
RÍ omthach, RÍ ofach, Rifach
Roach
Sadhbh, Sadbh, Sabhbh, Sadb, Sive, Sabia, Sabina	Saidhbhín?	'sweet/goodly'
Saoirse, Saiorse	 	'freedom' (mod.)
Samhaoir
Samthann
Sárnat
Scathach
Scáthdercc
Sciath
Sílbhe
Sinech
Sláine, Slania, Slanie
Sodelb
Sogáes
Sorcha	 	Brightness
Sósaidh
Stediana
Taillte, Tailltiu
Tanith
Tathan
Teamhair, Temair, Tara	 	'eminence/high place'
Teath
Téiti
Téitl
Tlachtga
Treasa (Teresa)	 	'strength'?
Tuilelaith, Talulla	 	abundance + lady
Úna, Oona, Oonagh, Una	 	'úna' hunger or 'uan' lamb"""

let clast = """Adair
Addis
Agnew
Ahearn, Ahern, Aherne	O'Echtighearn
Aird
Aitken
Anglum
Antisdel
Armitage, Armytage, Armidage, Armedy	O'Airmeadhaigh
Athey
Auchinlek
Autrey
Baggett
Bain
Baird
Ball
Ballagh
Ballantyne
Bambrick
Banan, Bannan, Bannon, O'Bannen	O'Banain
Banff, Hogg, Hogge	Mac an Bhainbh
Barde
Barken
Barkey
Barret, Barrett
Barrington, Barron?	O'Barrain
Barry, Berry	 	Norman 'de Barri'
Battle
Bean, Gilwaine, (White)	MacGilli-Bhain
Beatty
Begg, Beggs
Begley, Bagley
Behan, Beahan
Beirne
Bernnan
Berrigan
Bestick
Beyers
Biggar, Begar, Bera	O'Bearga
Binchy
Binsley
Bird
Blair, Blaire
Blevins
Blighe	O'Blighe
Blow
Boehn
Bohan
Boland	O'Beollain
Bolger
Bonner
Boone
Boran, Boren, Borin, Boring, Bouring
Bouey, Boughy, Boey
Bourns, Burns	O'Conboirne, MacConboirne
Bowler
Boyce
Boyd, Boyde
Boylan
Boyle, O'Boyle, Beahilly, Beale, Beatley	O'Baetheghaile
Braden, Bradin
Bradley
Brady	O'Bradaigh?	descendant of Bradach
Brahan	O'Brachain
Brangan, Brannigan
Brann
Bray	O'Breadhdha
Brazael, Brazil
Breen, McBreen	O'Breen
Brennan, Brenan, Brennen	O Braonain, O'Braoinain	Desc of Braonain 'sorrow'?
Breslin, Brislane	O'Brislain
Bresnahan
Brickin	MacGilli Briein
Broder, Bradner, Brothers	O'Bruadair
Brodie, Brody, Brodiff	O'Broduibh
Brogan	O'Brogain
Brophy, Broghie	O'Breithe
Brosnahan, Brosnan
Bruen, Brune, Bruin	O'Birn
Brynes
Brynnock
Buggy
Burk, Burke, Bourke, Burge	 	Norman French 'de Burgh'
Burnes, Burns, Byrne, Byrnes, Byrns (Lester, Lyster, Warren)	O'Burn, O'Byrne, O Broin	Desc of Broin
Cadden
Cadogan
Cady
Callaghan, Callahan, Calahan, O'Callaghan	O'Kelaghan
O'Ceallachain
Callan, Callanan, Callinan
Camden, Caden	O'Camdhain
Canavan, Canovan
Cannan, Cannon, Kanan?, McCannon
Canty
Carey, Cary, Carrie	O'Ciardha	descendant of the Dark One
Carlin
Carmichael
Carney
Carolan, O'Carolan	O Cearbhall« in
Carton, MacCarton
Carvel
Casey, O'Casey	O'Cathasaigh	desc of Cathasach 'watchful'
Cash, Kilcash	MacGilla Chaise
Cashion
Caskey
Cass
Casserly
Cassidy	O'Caiside
Catny, Kane	O'Caithniadh
Cavin
Cayley
Childers
Claffey
Clancy	Mac Fhlannchaidh	son of Flannchadh
Cleary, Clery, Clary, Clear, O'Clery, McClary, McCleary, MacCleery	O'Cleirigh	Desc of the clerk
Clingan
Cloherty
Clohessy
Cloony
Close
Clune, Clyne
Coady, Cody
Coakley
Cochran, Cochrane
Coffee, Coffey, Cowhig, Cuffe?	O'Chobhthaigh, O'Cobhthaigh
Coggins
Coholan
Colahan
Colclough, Colcroft
Coleman, Coalman	O'Clumain, O'Columain
Colgan, MacColgan
Colley
Collins, Collings	O'Coilen
Colvin
Comer
Comerford, Comsford
Comey
Comiskey, Comisky, Cummisky, McComish?
Conaghan
Conalty
Concannon
Condon, Condron
Conheady
Conklin
Conlan, Conlin, Conlon, Colwan	MacConluain
Conn
Connaughtan, Conattan	O'Connachtain
Conneely, Coneely, Connelly, Connole, Connolly, Conley, Conly	MacConghaola, O'Conghaile
Connel, Connell, O'Connell, O'Connel, MacConnal, McConnell	O'Conaill
Connich
Connor, Connors, Conner, O'Conner, O'Conor, Conyers	O'Conchobhair
Conroy, Conry, Connery, Mulconrey	O'Maoilchonaire
Considine
Conway, Convy	O'Conbhuidhe
O'Conaighain
O'Connowe
Coogan, Cowgan	O'Comhghain
Coolacan, Coolahan	O'Culaghaine
Coonan, Quinan	O'Cuanain
Cooney
Corbett	O'Cornain
Corcoran
Corey, Corry, Corr
Corkin
Corliss
Corner
Cornynn
Corrigan
Cosby, Foote	MacCosby	Cos - foot
Cosgrave, Cosgriff, Cosgrove, Costello, MacOscar?	O'Chosgraidh
Costigan
Cotter
Cottle	O'Coitil
Cottrell
Coughlan, Coughlin, Coglan
Coulter
Counagh
Counahan, Counihan
Cournane
Cowley
Coyle
Coyne, Koin, Kean, Keane, Kane, Kain, Cain, O'Keane, O'Kane, O'kane	O'Cain, O'Koin
Cragin
Creagh, Cregg
Creary
Creaven
Creedan, Creed	O'Criadhen
Creegan, Cregan, Creagon, Crehan, Crean
Cremen, Cremin, Cremins, Crimmins
Cribban	O'Cribbain
Crilley, Crilly, Krily
Croak, Croake, Croke, Croker
Croghan
Cronan, Cronin
Crosbie, Crosby	MacCrossan
Crosgrove
Crossin
Crotty
Crowley, Crawley
Cruise
Crummer
Cuddihy, Cuddy
Culhane, Cullen, Cullinane
Culley, Cullie
Culliton
Culloty
Cummin, Cumming, Cummins, Cummings	O'Cumin
Cunniam, Cunnion, Canning	MacCoinin
Cunniff
Cunningham, Conyngham, Cunnigan	O'Cindellain, O'Conagan, O'Conaighain, O'Connaghain, O'Congadhain
Curley, Kirley
Curnane
Curran, Currin, Curren	MacCarrain
Currie, Curry, Cury, O'Curry	Curoi?
Curtin
Curtis, Curtiss
Cusack, Cusick
Cushing, Cussen
Cuskley	MacGillicuskly
Dacey
Dagan
Daly, Dailey, Daily, Daley. O'Daly	O'Dalaigh	Desc of Dalach ('assemblyman'?)
Darby
Dardis
Dargan
Darmody
Darragh
Daunt
Davoran
De La Mare, Delamore
De Loughry, Delury
Deady
Deahl
Deasy
Deeney, Deeny
Deery
Defferary
Deghy
Delahunty, Dolohanty
Delaney, Delany
Delvin
Dempsey
Dennehy, Danaher, Danahy, Danihey
Derrick, Derrig	O'Deirg
Desmond
DeVaney, Devaney, Devany, Duane, Duany	O'Duanmhaigh
Devereaux, Devereux, Devery
Devine, Devinn
Devlin
Devrin
Dewey
Diamond, Dimond
Diarmid
Diegnan, Dignen
Digney
Dillon
Dinneen
Dissett
Dixon	O'Discin
Doble
Docherty, Doherty, Daugherty, Dougherty, O'Doherty
Dockery
Doheny
Dolan
Donaghue, Donoghue, Donahue, Donoho, Donohue, O'Donoghue, Donachie, Donaghy	O'Donchadha, O'Donnchadha
Donegan
Doney, Denny	O'Duineadhaigh
Donlan, Donlin, Donlon
Donlevy
Donnellan, Donelan
Donnelly	O'Dunghaile
Donovan, Donavan, O'donnovan, O'Donovan
Doody
Doogan, Doohan, O'Dugan
Doolan
Dooley
Dooling
Doon, Dooney
Doraghy
Doran, Dorrian
Dore
Dorgan
Dornan
Dorsey, Dorcey, Dorcy, Darcy, D'Arcy, Darkey, Dasse?	O'Dorchaidhe
Dowey
Dowglas
Dowling, Du Laing, Laing, Dulin?	O'Dowling
Downer, Downes, Downey
Doyle, Dougill	O'Dubhghaill, O Dughghaill	Desc Of Dubhghall
Drew
Driscoll, O'Driscoll
Drohan
Drum
Drummond
Duff, Duffey, Duffy	 	'black/dark'
Dufficy
Duvegan, Dugan, Duggan, Duignan, Dougan, Doughan	O'Duibhagain
Drum, Drummond	O'Drum
Duhig, Duignen
Dulleran	O'Duibhleargain
Dunkin	O'Duinchinn
Dunn, Dunne, Dun	O Duinne	desc of Duinn (brown)
Dunphy
Dunwoody, Dinwiddie
Durack, Durick, Durrick, Durrig	O'Duibhraic
Durkin	Mac Dhuarcáin?
Durning
Dwyer, O'Dwyer
Dyal
Dynes
Earles, Earley
Egan, Eagen
Ellig
Eurell, Yourell
Fagan
Faherty, O'Faherty	O'Faghartaigh
Fahey, Fahy
Fallon
Fane
Fanning
Faricy
Farley
Farnan
Farner
Farran, Farren, Heverine	O'Fhuathmharain
Farrell, Farrall, Farrelly, Ferrell, O'Farrell	O'Fearceallaighe
Faucett, Fausset
Fay
Fayre, Milford	O'Maoilfaghmhair
Fealy, Feeley, Feely, Fehily, O'Feely, Pickley	O'Feehily
Fearon
Feehan
Feeney, Feeny	O'Feinneadha
Feennell
Feherty
Fennelly
Fennessy, Finnessey
Fenney
Ferney
Ferrick, Ferris, Ferry
Fessey
Finan, Finnin, Fynan, O'Finan	O'Fionain
Finn
Finnegan, Finnigan	O'Fionnaghain
Finneran	O'Finntigheam
Finnerty
Finsley
Finucane
Fitzgerald
Fitzgibbon
Fitzmaurice
Fitzmorris
Fitzpatrick
Fitzsimmons, Fitzsimons
Flaherty, O'Flaherty
Flanagan, Flannagan, Flannigan	Mag Flannagain
Flannelly	O'Flannghaile
Flannery	O'Flannabhra
Flatly, Flatilly	O'Flaitile
Flattery	O'Flaithri
Flavin
Flood, Floody
Flynn, O'Flynn	O'Floinn	Desc of Floinn (flann - ruddy)
Fogarty
Folan
Foley	O'Fodhladha
O'Fuala
Folgan
Follis
Foody, (Swift)	O'h-Fhnadha
Foorde, Ford
Foote	O'Trehy
MacCoshy
Forbes, Firbis	MacFirbis
Forhan, Foran, Fornan
Forristal
Foskey
Foy
Frawley
Freeney
Freethy
Frenshe, French, Ffrench	O'Fraechain
Friel
Friery
(Fuller, Fowler)	O'Fualairg
Furey
Furlong
Furphy, Furfy
Fursey
Gaff, Gaffey, Gaffney
Gaffigan
Galbrath
Galligan, Gallagan, Gealan, Geelan	O'Gealigain
Gallager, Gallagher, Gallaher
Gallahue
Gallivan
Galvin
Gambell, Gamble	O'Maoilchluiche
Ganly	Mac Anluain
Gannon, Ginnane	Mag Fhionnain
Gant
Garigher
Garr
Garrahan
Garvey, Garvy, McGarvey
Garvin
Gasaway
Gately, Keightley, Catley	O'Gathlaoich
Gatens
Gaughan, Gahan	O'Gaibhtheachain
Gavaghan, Gavigan
Gavin
Geary, Gerry, McGeary	O'Gearadhain
Gehan, Gahan, Gettins	O'Gaoithin
Genty
Geoghan, Geoghegan, Gegan
Geraghty, Gerrety, Garraghty, Garrett, Garritty, Heraghty, Herity, Harrington	O'h-Oireachtaigh, O'h-Heraghty, MacGeraghty
Ghaney
Gibney
Gielty
Gilbride
Gildae, Gilday, Gildea
Gilfillon
Gilfoyle, Guilfoyle, MacGilfoyle, Paul	MacGiolla Phoil
Gilhooly, Gilhuley
Gillan, Gillen
Gillespie, (Bishop)	MacGiolla Easpaigh	Son of the follower of the bishop
Gillham
Gilligan, Gilgun
Gillin
Gillmor, Gilmer, Gilmore	MacGilimir
Gilmartin	Mac Giolla Mhartain	Follower of St. Martin
Gilpin, Gilfin, Gill	MacGillifin
Gilroy	Mac Giolla Ruaidh	'Son Of The Red-Haired Lad''?
Gilsenan
Giltinan, Giltinane
Ginnaty
Glancey, Glansey
Gleason, Gleeson
Glennon, Glenin, Glinn, Gloin, Glynn	O'Gloinin
Goddan, (Godwin)	O'Gadain
Godsil
Gogarty
Goggin
Gollogly, Gallogy, Ingoldsby	MacGallogly
Maclnogly
Gorey
Gorham
Gorman, O'Gorman, O'Garman?	O'Gormog
O'Gormghail
Gormilly,Gormally, Gormley, Grimley	O'Goirmghiallaigh
O'Gairmleadhaigh
Goslin
Gough, Goffe, Goff, McGough	O'Cuaghain
Gourley
Governey
Grakane
Granard
Graney
Grannan
Gratten
Graulty
Greaney, Greany
Greehy
Greeley
Greer
Gregan, Greghan, Grehan, Crehan, (Graham)	O'Creachain
Gregg
Gribbin
Griffen, Griffin, Griffy	O'Griobbtha
Guinan
Guiney
Gunning	O'Conaing
Gurnett
Guthrie	O'Lahiff
Hackett
Hadden
Haffey
Hafner
Hagan, O'Hagan
Halley, Hailey, Hale, Haley, Ally	O'h-Ailche
Haliday
Hallahan, Halligan
Hallinan
Halloran, Hallaren, O'Halloran
Halpin
Hamill, Hammill	O'h-Aghmaill
Hand	MacLave	Lamh hand
Handran
Haney
Hanifan, Hanifin
Hanley, Handley, Hanly, O'Handley
Hanlin, Hanlon, O'Hanlon
Hanna
Hannan, Hannon
Hannigan
Hannity
Hanrahan, O'Hanrahan
Hanratty
Haran	O'h-Arain
Harden
Hardiman
Harkin, Harkins
Harney
Harrigan
Hart, Heart, O'Hart
Hartigan
Hartnett
Harty
Harvey
Hatchell
Hatty, Hetty	O'h-Aidith
Haugh, Haughee, Hoy, Hoey, Hawe, Howe	O'Haughey
Haughion	O'h-Eochagain
Hay, Hayes, Hays, Hughes	O'Hay
Heaney, Heany, Heagney?	O'h-Eana
Healey, Healy
Heanaghan	O'h-Eidhneachain
Heasty
Hefferman, Heffernan
Hegarty, Hegerty, Hagerty, Haggerty
Heggessy
Hellam
Hellon
Hellowell, O'Hellowell
Helvick, Helwick	O'h-Oilmhec
Henchion
Henebry
Henly, Hennley
Henn	O'h-Enda
Hennaghan. Heneghan
Hennessey, Hennessy, Hennesy
Herlihy, Hurley
Hernan, Hernon	O'h-Iarnain
Herne, Hearn, Hearne, Hurn, O'Hearn, O'Hern	O'h-Eimhirin
Heron
Heslin
Hetherington
Hett
Hickey	O'hIceadha	Desc of the healer
Hinchey
Hinchion
Hoar, Hoare
Hoban, Hobin	Muntir Ubain
Hogan	O'Hogain?
Hoke
Holland
Holley, Holly
Holligan, Holian
Holohan, Holohan
Horan, Hamran	O'h-Uathmharain
Horgan, O'Horrigan?
Hough
Houlihan
Houneen, O'Houneen, O'Honeen, (Greene)	MacUaithnin?
Hourahane, Hourigan
Hughes	O'Aodha
Hurley, Harley	O'Hurley
Hurst
Hussey, Oswell	O'Hease
Hyland
Hynes, O'Heyne	O'h-Eidhin
Ivers, Ivors, McIvor, (Howard)	O'h-Iomhair, Maiomhair
Jardan
Kane	O'Cathniadh
Kavanagh, Kavanaugh, Kavangh, Kaveny, Cavanagh, Cavanah, Cavanaugh, Keveny, Kevin, Cowen, Cohen, Coen	O'Caomhain
Keane, Kane, Kyan, O'Keane	O'Cathain
Keaney
Kearn, Kearns, Kearins	O'Ceirin
Kearney, Carney	O'Cearnaigh
Keary, McKeighry	MagFhiachra
Keating, Keatinge, Ceitinn	O'Ceadfhada
Keaty	O'Ceathaigh
Keedy
Keegan, Keeghan, Cockane	MacCiochain
Keelan, Keelchan, MacCallin (Coward)	MacCailleachain
Keeler
Keelty, Kelty, Kielty, Kilty
Keely, Kealey, Keily, Kiely, Kaely, Cayley	O'Caolloaidhe
Keenan
Keevan
Kehoe
Keirans, Kieran
Kellahan
Kellegher, Kelleher
Kellett
Kelly, Kelley, O'Kelly	O'Ceallaigh, O'Ceallachai? Muintir Ceallaigh	Desc of Ceallaigh (ceallach strife)
Kenehan
Kenn, Kenna
Kenneally, Kennealy, Keneally, Kenealy, Kennelly, Kenelly
Kennedy	O'Cinneide	Ceann head, eidigh ugly
Kenney, Kenny
Kennyon, Canning	MacConin, MacCoinin
Keohane
Keough, Keogh, McKeough
Kernaghan	O'Cearnachain
Kernan
Kerr	 	NF
Kerrigan	O'Ciaragain
Kerrin	O'Ceirin
Kerruish
Kevigan	O'Caemhagain
Keyes
Kilbain
Kilbride, Kilbridge
Kilcullen
Kilduff, Gilduff	Mac Giolla-Duibh
Kilfeather
Kilgallon
Kilgannon
Kilgore
Kilkelly, Killikelly	Mac Giolla-Cealaigh
Kilkenny	Mac Gilla Kenny
Killeen, Killin, Culleen	O'Gilin
Killick
Killpatrick, Kirkpatrick?	Mac Giolla Padraig	Son of the follower of St. Patrick
Kilmartin
Kilmeade
Kinahan
Kinealy	O'Cinnfhaelaidh
Kinnane
Kinnavy (Bones)	O'Cinnchnambha
Kinsella
Kirby	O'Ciarmhaic
Kirwan, Kerwin
Kivlahen, Kivlehan
Kneafsey
Kyle
Lacey, Lacy
Laffan
Laing
Lalley, Lally
Lamb, Lambe	O'Luane
Lambkin
Lanaghan, Lanahan, Lannigan, Lanigan, Lanergan, Langan, Lannen, MacClanaghan?	Mag Lannagain
Landregan, Landrigan
Lane, Lahan?	O'Laoghain
Lappin
Largay
Larkin, Larcom	O'Lorcain
Latteray
Laughead
LaVelle, Lavelle	O'Mullaville
Lavender
LaVerge
Laverty, Lafferty
Lavery	O'Labhrahadha
Lavin
Lavy, Laffey, Lahey?
Law
Lawder
Lawless
Lawlor, Lawler, Lalor
Lawn
Lawton
Leachán, Lehan
Leasy, Larrisy	O'Learghusa
Leavy, Levy
Ledwich, Ledwick, Ledwidge
Lee	O'Laodhog
Leehey, Leehy, Leahy
Leigh, Lye	MacLaighid
Lemon
Lenergan
Lennon, Lannen (Leonard)	O'Leannain
Lennox
Lenty, Lente
Leraton
Lester
Liddane
Liddy, O'Liddy	Muintir Lideadha
Liggan
Lilly, Leach, Leech, Leitch	O'Laechaille
Linighan, Linnegan, Lenihan, Linehan
Linnane
Linton
Loane, Olohan?	O'Luain
Lockhart
Logan
Logue
Lomasney
Lonergan, Londergan
Longhan	O'Lochain
Looby, Luby
Loody, Ludy	O'Luachduibh
Looney
Loounane
Lord
Lorden
Lorigan, Largan, Legge	O'Lairgnen
Louden
Loughnane, Loughnam, (Loftus)	O'Lachtnain
Loughran, Loughrin
Lowry, Lowery
Lucey, Lucy
Lundie
Lundragon
Lunney
Lunshekaun	O'Loingseachain
Luxom	O'Luachaim
Lyden
Lydican	O'Laighdiachain
Lydon
Lyman
Lynagh
Lynam
Lynsky, Lynch	O'Loingsigh, MaGloinsg
Lyons	O'Liathain
Lyttle, Little	O'Laitile
Macaleese
MacAlevy
MacAllen
MacAllister
MacAnally, McAnally, MacNally, McNally
MacAndrew, McAndrew McAndrews, Andrews	MacAindris
Macaninch
MacArdle, McArdle
McAsey
MacAteer, Macteer
MacAuley, Macauley, McAuley, McCauley, McCauly, McCawley, McAuliff, McAuliffe, Macauliffe
Macavaddy
McAvay, McAway
MacAvee, Kilboy, Gilboy	MacGilli-Bhuidh
MacBrehon	 	judge
MacBride, McBride
McBurney
MacCabe, McCabe
MacCafferky, MacCafferty
MacCaffrey, Caffrey, McCaffery, McCaffrey, Caffrey
MacCall, McCall, Call
McCamly
MacCann, McCann, McCanna, MacCana?
McCarl
MacCarron, MacCarroon, MacCarhon, Carron, Carson, Carse?	MacCarrghanma
MacCarthy, McCarthy, MacCartney? McCarty, Carthy, Carty	Mac Carthaigh	Carthach loving
MacCarrick	MacConcathraigh
MacCaughan
MacCaw
MacCecht
MacCiochain
McClatchey
McClay
McClelland, Cleland
MacClintock
McClinton
McClire
McClory
McClughan
MacClumpha
MacClure, McClure
MacCluskey, MacCloskey, McCloskey, MacClosky, McCluskey, Closky	MacBlosky
MacCochlann
McCollum
____	MacConleitrech
MacConaughey, McConaughey
McConnville, McConville
MacConrai
MacCormack, MacCormick, McCormack, McCormick, McCromick
MacCorquodale
MacCourt, MacCort, McCord
MacCoy, McCoy	 	'Son Of Aodh'
MacCracken, McCracken
MacCready, Macredie
MacCruddan, MacCrudden, Cruden
MacCuill
MacCulloch, MacCullough, McCulloch, McCullough
MacCune
MacCuolahan
MacCusker, Mccusker
MacDaid
MacDermott, McDermott, Carmody, Kermode, Kermody, Diarmid	O'Duibhdiorma
MacDevitt, McDevitt, Devitt
MacDonagh, MacDonough, McDonough
MacDonald, McDonald
MacDonnell, McDonnell	Mac Domhnaill
MacDowel, McDow?
MacEachern
MacElderry
McElhatton
McEllgunn
MacElligot, MacElligott
MacEllistrim
MacEnery
McEnhill
MacEnroe, MacEnchroe
McEntee
MacEochagan
MacEvilly
MacEvoy, McAvoy, McEvoy,
MacFadden, McFadden
MacFall
MacFarland, Macfarlane, Parlan	MacPartholain, Mac Pharlain
McGaghran, McGaughren
McGalloway, Galwey
MacGann
MacGarrigle
MacGarry, McGarry, Garry
McGaughy, McGahey
McGauley, Gaule
McGavisk
McGaw
MacGee, McGee, Magee, MacGehee?	MagAedha
MacGeogh, McGeough
MacGil, MacGill, Magill, Gill	MacGiolla
MacGillycuddy, McGillicuddy
McGilton
MacGinley, McGinley, McKinley
MacGinn, McGinn, McGing
McGinty, McGinity
McGivney
MacGlashan
MacGlinchy
MacGlynn
MacGolderick, MacGoldrick, McGoldrick, Goderich, Golding, Goulding, Golden, Goldrick, Waller	MacUlahairg
McGonigal, McGonigie
McGourty
MacGovern, McGovern
MacGowan, MacGowran, McGowan, O'Gowan, MacGoohan? Gowran	Mac an Ghabhann	smith
McGrail, McGreal
MacGranahan, Granaghan, Granahan
MacGrane, MacGreine
MacGrath, McGrath, MacGraw, McGraw
McGreevy
MacGrogan, Grogan
McGuckin, Guckeen, Guckian
McGuigan, Maguiggan, Goodwin, Godwin	MacGuiggan
MacGuinness, Macginnis, McGinnis, MacGenis, McGuinness, Guiness
MacGuire, McGuire, Maguire, Macgwire
MacGuirk, McGuirk
McGunn
MacHoneen, Green, Greene, Tonyson, Tennyson	MacUaithnin
MacHugh, MacQue, McHugh, McCue
McIlmail, McIlmale
MacIlrea, Kilrea (Gray)	MacGilli-Riabhaig
MacIlroy, McElroy
MacIlvany
McIlveen
MacIlvenna
McIlwee
MacInerney, McInerney
MacIntyre, McIntire, McIntyre
MacInver, Maginver, Gaynor	MacFinnbhair
Mack
Mackay, Mackee, Mackey, MacKey, McKay, McKee
McKean, McKane
McKechnie
McKeever, McGeever
McKeighry
MacKelvey, McKelvy
Macken	MacCuinn
MacKenna, McKenna	Mac Cionnaith
MacKeon, Mackeone, Mackeown, McKeown, MacOwen	MacEoghain
McKiernan, Kiernan
MacKinnawe
McKinney, Kinney
McKinnon
McKitrick, McKittrick
McLaren
MacLarty, Larety?
MacLaughlin, MacLoughlin, McLaughlin, McLouglin, Magloughlin, O'Loughlin, O'Laughlin, Loughlin	Mac Lochlainn
MacLean, McLain
MacLysaght, Lysaght
McMaham
MacMahon, McMahon, McMahan, (Matthews), Fitzursula?
McManamin, McMenamin
MacManus, McManus
MacMathan
McMerriman, Merryman, Merriman	MacGillimore
MacMonagle
McMullen, Mullan, Mullane, Mullen
MacNamara, McNamara
MacNamee, McNamee, Meath, Mee
McNaughton, Naughton
MacNebo, MacNaboe	 	'victory'
McNeese, MacNeice
MacNeill, MacNeil, McNeil	Mac Neill	Son of Neill
McNellis, McNelis, McNelly, Nellis	Mac Niallais
McNerney
MacNevin, Nevin
McNicholas
MacNulty, McNulty
MacParlan, McParlan, McParland, McParlin, Bat, Bats, Batson, Bateson	MacPartholain
McPike, Pike
McQuade, Quaid
MacQuilkin, MacCulkin?
MacQuillan, MacQuillen, McQuillan
MacQuilly
MacQuirk, Quirk, Quirke
MacRaith
MacReamoinn
McRickard
MacRory, MacCrory, McCrory, McCrary
MacShane, McShane
McSherry
MacSorley, McSorley, McSloy?
MacTague, MacTeig, McTigue, (Montague, Montagu)	MacTaidhg
MacTavish, MacIltavish	MacGilla Samhais
MacUlhaney, McElhinney
MacVeigh, McVeigh, McVeigh
McVerry
McWay, McWey
McWilliams
Madden
Madigan
Magauran
Magettigan	O'h-Eitegein
Magilsinan	Magilsitnan
Magnan	O'Maghnain
Magoran, Orume, Orme	Mac Odhrain
Magunshinan
Mahaffy
Mahaney
Maher, Mahar
Mahonen
Mahoney, O'Mahoney, O'Mahony, Mahonny, Mahorney
Maines
Makens
Malcan, (Singer)	O'Maoilcana
Malia
Mallin, Mallon, Mallen	O'Mailina
Malloy
Malone	O'Maoileoin
Maloney, Malony, Malonny, Mullowne, Mullowney, Mullooney	O'Maoilbloghain
Mandy
Mangan, Mongan, Mungen	O'Mongain
Manley, Manly
Mannion, (Manning)	O'Mainnin
Mannix, Manix
Markam, Horseman, Ryder	O'Marcachain
Markey, Markley
Maroney
Marr
Marren
Maskey
Massey, Massie
Matchett
Mawhinney
May
Mayclin
Mayo
Meachar
Meade
Meagher
Meehan	O'h-Emeachain
Meekin
Meenehan, Meenan? Minahan, Mynahan	O'Muimhneachain
Megahey
Megarity
Meighan
Melaven
Mellan, Mellon, Mellen	O'Mellain
Melloy, Meloy
Melody
Menagh
Menton
Mergin, Bergin, Bergan, Bergen	O'Aimirgin
Mernagh
Meyler
Mhaolain
Miley
Millan
Millett
Milford	O'Mulfover
Millia
Milligan
Millikin
Minikin, Minnick
Minto
Moghan, Mohan	O'Mochain
Molan
Molina	O'Maoilfhiona
Molloy
Molone, Moloney
Monaghan, Monahan, Monohan, Mongan	O'Muineog
Monarty
Monteith
Moody
Moonen
Mooney, Meeny	O'Maoinaigh
Moraghan, Morahan
Moran	O'Mughroin
Morell
Morley	O'Murgally
Moriarity, Moriarty, Moriaty
Moroney, Moroni
Morrin	O'Morain
Morrissey, Morrisy	O'Muirgheasa
Morrow
Motley, Mottley
Moyles, Moiles, Moles
Moynagh	O'Maonagh
Moynihan
Mugan
Muintirceallaigh
Muintirlideadha
Mulally
Mulcahy
Muldoon, Muldown	O'Maoilduan
Mulgrew, Mulgrue
Mulharen, Mulhearn, Mulheron
Mulholland, Mullholan, Mullholand	O'Maoilcallain
Mulkeen
Mulkeerrinn
Mullally
Mullaney
Mullarkey
Mullattin, Molohan, Molohon	O'Maoilaithin
Mulligan, Mullgan (Baldwin)	O'Mulligan
Mullin, Mullins
Mulratty, Malet, Ratten, Rait	O'Maoilraite
Mulrenan, Mulrennan	O'Maoilbhrenain, O'Maoilbhreanainn	Follower of St Brendan
Mulrooney, Moroni	O'Mulrony
Mulroy, Roy	O'Maoilruaidh
Mulroyne	O'Maoilruain
Mulvaney, Mulvanney, Mulvany
Mulvey
Mulvihil, Mulvihill, Mulvehill
Mundell
Mungavan
Munley
Murdock
Murnaghan, Mynahan
Murphy, Morphie, O'Murphy, MacMorrow, MacMarray, MacMurray, MacMurrough, Morell	O'Murchadha, O'Murchada, MacMurchada	Murchadh sea-warrior
Murray	O'Muireadhaigh
Murtagh, Murtaugh, Murty, Mortimer
Mylett
Myrick
Nagle
Nangle
Nash
Neal, Nealon, Neilan
Neary
Nee
Nelligan
Nesbit, Nesbitt
Nesdale
Nimmons
Nogan
Nohilly
Nolan, Noolan, Nolin, Nowlan	O Nuallain, O'h-Uallachain, MacUallachain, MacCuolahan,	Desc Of Nuallan
Noone, Noon, Noonan, Nuana	O'Nuadhain
Norris
Nugent	MacGunshenan, Magunshinan
Nunnally
O'Biernes
O'Boye
O'Brien, Brine	O'Briain	Desc of Briain (Brian Boru?)
O'Briun, Briun, Brin	O'Beirne
O'Buhilly
O'Carroll, Carroll
O'Corráin
O'Cuinneagain
O'Cunig
O'Dea
O'Donall, O'Donell, O'Donnell, O'Donnell	O'Domhnaill
O'Dowd, O'Dowda, Dowd, Dowdy, Doud	O'Dubhda
O'Faelechoin
O'Falvey
O'Faolain
O'Flarety
Ogan
O'Gara
O'Grady, Grady
O'Hara, O'Harra
O'Hardy
O'Hayer, O'Hare, Hare	O'h-Ir, O'Hir
O'Hehir, Hehir, O'Hea, O'Haire, O'Hare, Hare	O'h-Aichir
O'Hilly
O'Hiskeen, O'Histeen, Hastings	O'h-Uisgin
O'Hora, O'Hore
O'Horrigan
O'Howen, Owens	O'h-Eoghain
O'Keefe, O'Keeffe, Keefe
O'Laodhog
O'Leary, Leary
O'Malley, Maley
O'Meara, O'Mara, Meara, Mara	O'Mearadhaig
O'More	O'Mordha
O'Mulloy, Mulloy
O'Neal, O'Neil, O'Neill, Neele, Neely	 	Desc of Neill
O'Riordon, Riordan, Reardon, Readon
Orman, Orr
O'Rourke, O'Roark, Rourke, Roark, Rooke	O'Rourke
O'Shea, Shay, Shea, Shaw	O'Seagha
O'Seghdha
O'Sullivan, Sullivan, Silver, Silvers	O'Suilleabhain, O'Suileabhain	Suil - 'eye' + Levan, a Celtic god
O'Toole
O'Trehy
Pace
Padden, Paden
Paibht
Palmer	O'Mulfaver
Parkhill
Parle
Patton
Payne
Pender
Perry
Phelan	MacGiolla Fhalain
Pierce, Peirce
Plannery
Plunkett
Power, Powers
Prendergast, Pendergast
Price?	O'Luachain
Punty
Quaine, Quan	O'Cuain
Qualey
Quealey
Quigg
Quigley	O'Coiglidh
Quilligan
Quinan
Quinlan
Quinlivan
Quinn	MacCuinn, O'Cuinn	Desc of Conn
Rabbit, Rabbitt
Radden, Ravan, Radwin	O'Radubhain
Rafferty	O'Rabhartaigh
Rafter, Raftery	MacReachtagain
Rainey
Rattigan, Ratigan, Rhatigan
Ratty
Rea
Regan, Reagan, O'Regan	O'Riagain
Reavey
Redden, Reddin
Redican
Reed, Reid, Reedy, Ready, Reidy
Reen
Reilly, Riley, O'Reilly, O'Rielly, O'Reiley, O'Rahilly	O'Ragailligh	Desc of Ragaillach
Relihan
Renehan, Ranahan
Riddle
Rigney
Roach, Roache, Roche
Robey, Robie, Roby
Rock, Rocks
Roddy? (Rogers)	O'Ruadhraigh
Roden, Grayden	Mag Rodain
Rogan, Rohan
Rolan, Rollan, Rollin, Rowlan, Rowley, Rowland	O'Rothlain
Ronan, Ronayne	O'Ronain
Roody
Rooney
Rossiter
Rowells, Rowles
Royle
Ruane, Rowan, Roan	O'Ruidhain
Rusa
Rushe	O'Luachair
Ruther
Ryan, Murrian, Murrin	O'Mulrian, O'Malvilriain
Sallery
Salmon, Salmons, Sammon, Sammons
Sangster
Sankey
Saul
Scannell, Scanlon, Scanlan, Seanlan	O'Scannail
Scullin
Scully, Duscully, Scally?	O'Duibhscuile
Sebon
Seeny, Fox	O'Sionna
Seery
Semple
Sexton	O'Seisnain
Sewell, Walker	O'Sewell
Shade	O'Sedna
Shahan
Shalloo	 	(Fr Challoux?)
Shanahan
Shankland, Shanklin
Shanley
Shannon
Sharkey
Sharvan
Shaughnessy, Shoughness, Sandys, O'Shaughnessy	O'Seachnasaigh
Sheedy
Sheehan, Sheahan, Shehan
Sheehy
Sheil
Shelley, Shelly, Schelly, Schely
Sheridan
Sherin
Sherlock
Sherman
Sherwin
Shevlan, Shevlin
Shields, Shiels
Shine
Shortall, Shortell
Shorten
Shovlin
Sieferman
Silven
Silvers
Sims, Simmes, Simpson, Simkins, Simcocks, Simon	MacSimoin
Sinnott
Skahill, Cahill
Skelley, Skelly
Skerrett
Slattery
Slavin
Sloane
Smytherman
Snee
Sollers
Somers	O'Sumaghain
Somerville, Sommerville
Speers
Spellacy
Spellman
Spillaan, Spillaine, Spillane, Splaine, MacSpallane, (Spenser)	O'Spealain, MacSpeallane
Stack
Starkey
Staunton
Stoney	O'Mulclohy
Strachan, Strahan
Straffan
Sugrue
Sunagh, Swanny	O'Suanaigh
Suppel
Suthern
Sweeney
Synan	MacGila Sinin
Taaffe, Taffe, Taft
Tackit	O'Tackney
Taft
Taggart
Tallon
Tangney
Tansey
Tarpey, Tarpy, Torpy	O'Tarpaigh
Teague, Tighe	O'Teagha
Teeples
Teevans
Tegan
Tenehan
Tenpenny	O'Tiompain
Tiernan, McTiernan	O'Tighearnain
Tierney	O'Tighearnaigh
Tiffany, Tiffney
Timmins, Timmons, Timon, Tymon
Toal
Tobin, Toban
Toffey, Todd	O'Toghdha
Tolan, Toland, Thulis, Tollis	O'Tuathalain, O'Tuathlains
Tolbert
Tomelty
Toner, Tonner
Toody
Toomey, Twomey
Tormey
Torpy
Trant
Trautt
Travers
Traynor
Treacy, Tracy, Tracey
Trench
Troy	O'Turrain
Tully, Tilly, Tilley	O'Taichligha
Tunney
Tuohy, Tuhe, Tuhey, Touhy, Towey
Turley
Turvey
Tweedy
Tynan
Unckles
Uriell
Vail
Vallila
Veale
Waddell
Wahlen
Waldron
Wall
Walsh, Welch, Welsch, Welsh	 	A Welshman
Ward	Mac an Bhaird	Son of the bard
Warrington
Waterbury
Waters
Watt
Webb
Weeney
Weir
Whatley
Whelan
Wittlin
Wogan
Woods, Wood	O'Coilligh
Woulfe, Wolfe	MacTyre
O'Faelechoin
Wylie
Wynn, Wynne, Mulgeehy	O'Maoilgaoithe
Yeates	 	 """

let memale = """Aannapada	Alalgar, Alagar, Alapar, Alaparos	Alulim, Alurim, Aalu, Aloros
Amegalana	Ammeluanna, Ameluana, Ameluan, Amelun, Amelon	Anenlilda
Arwium	Atra-hasis, Atraharsis	Bahina
Barsalnunna	Buan	Damikilishu
Dumuzi, Dunuzi, Dauizi, Daonos	Enkidu	Enmebaragesi, Mebaragesi
Enmebuluga	Enmeduga	Enmeduranki, Eueduranki, Euedoraki, Euedorachos
Enmegalama	Enmenduranna	Enmenluanna
Enmenunna	Enmerkar	Enmeushumgalanna, Enmegalanna, Megalanna, Megalaros
Ennundaranna	Ensipazianna, Ansipizzianna, Anempisana, Amempsinos	Ensukushsiranna
Etana	Gilgamesh	Ibbisin
Iltasadum	Kalumum	Lugalkitun
Lugulbanda	Mashda	Melamanna
Melamkishi	Mesannapada	Meshe
Meskiagkasher	Nangishlishma	Puzur-Amurri
Samug	Sangasu	Sinmagir
Tirigan	Tizkar	Uan
Uanduga	Ubara-Tutu, Ubartutu, Ubiartu, Otiartu, Otiartes	Untash-Gal
Ur-Nammu	Urnungal	Urnungalak
Ut-napishtim	Utu	Utuabzu
Utuhegal	Ziusudra, Zisudra, Zisuthra, Xisuthros, Sisuthros	Zukakip
Abdalonymus	Abimilki	Achololim
Admago	Agbal	Anysus
Arabo	Asdrubal	Astegal
Baalhaan	Bomilcar	Boodes
Bostar	Carthalo	Eshmunazar
Fierelus	Fuabal	Fuano
Haggith	Hamilax	Hamilcar
Hamiscora	Hampsicora	Hannibal
Hanno	Hannon	Hasdrubal
Himilco	Hiram	Jabnit
Kandaulo	Luli	Mago
Maharbal	Mapen	Mathos
Merbal	Metallo	Mintho
Muttines	Salicar	Shipitbaal
Sirom	Tendao	Tetramnestus
Zaracas	Zinnridi
Abdi-Kheba	Abiditaan	Abiesuuh
Ammiditana	Ammisaduka	Apilsin
Arammadara	Arsaces	Asmadu
Balshazzar	Berossus	Bhhazuum
Burnaburiash	Daad..	Ditanu
Gezer	Heana	Ibni-Amurru
Ilima-ahi	Iptiyamuta	Kadashman
Maam..	Milkilu	Nabonidus
Nabunaid	Nabupolassar	Namhuu
Namzuu	Nebuchadnezzer	Ninus
Ninyas	Obares	Saamsuiluna
Sheshbazzar	Sinmubaliit	Sumalika
Sumula...	Suni..	Tattenai
Tuubtiyamuta	Yaamkuuzzuhalamma	Zabium
Zuummabu
Aakheperkare	Addaya	Ahhotpe
Ahmes	Ahmose	Ahmose-saneit
Ahmose-sipari	Akencheres	Akunosh
Amenakht	Amenakhte	Amenemhat
Amenemheb	Amenemopet	Amenhirkopshef
Amenhirwenemef	Amenhotpe	Amenmesse
Amenmose	Amennestawy	Amenope
Amenophis	Amenwahsu	Ameny
Amosis-ankh	Amoy	Amunemhat
Amunherpanesha	Amunhotpe	Anen
Ankhef	Ankhefenamun	Ankhefenkhons
Ankhefenmut	Ankh-Psamtek	Ankhsheshonq
Ankhtify	Ankhtyfy	Ankhu
Ankhuemhesut	Any	Apophis
Baba	Baenre	Bak
Bakenkhons	Bakenkhonsu	Bakenmut
Bakennefi	Bakenptah	Baky
Bata	Bay	Bek
Bengay	Besenmut	Butehamun
Denger	Deniuenkhons	Djadjaemankh
Djau	Djedefhor	Djedhor
Djedhoriufankh	Djedi	Djedkhonsiufankh
Djedkhonsuefankh	Djedptahefankh	Djedptahiufankh
Djehutmose	Djehuty	Djehutymose
Djenutymes	Djeserka	Djeserkare
Djeserkheprure	Djesersukhons	Djethutmose
Djhutmose	Genubath	Gua
Haankhef	Hapimen	Hapu
Hapuseneb	Hapymen	Haremakhet
Haremsat	Harkhebi	Harkhuf
Harmhabi	Harnakhte	Harsiese
Hay	Hemaka	Henenu
Henuka	Heqaemeheh	Heqaib
Herenamenpenaef	Herihor	Hesire
Hor	Horapollo	Hordedef
Horemheb	Hori	Hornedjitef
Horpais	Horwebbefer	Hrihor
Hunefer	Huy	Huya
Iawy	Ibana	Ibe
Idy	Ikeni	Ikui
Imhotep	Inarus	Inebni
Ineni	Inyotef	Ipi
Ipuwer	Ipuy	Ipy
Ishpi	Iu-Amun	Iufankh
Iufenamun	Iunmin	Iuseneb
Iuwlot	Iyerniutef	Iyimennuef
Iymeru	Jarha	Kadjadja
Kahma	Kaka	Kanakht
Karnefhere	Katenen	Kawab
Kay	Kemuny	Kenamun
Kenefer	Kerasher	Kha
Khaemhet	Khaemnetjeru	Khaemwaset
Khahor	Khakheperraseneb	Kha'y
Khensthoth	Kheruef	Khety
Khnemibre	Khnumhotep	Khnumhotpe
Khons	Khonsirdais	Khonskhu
Khonsuemwaset	Khufukhaf	Khui
Kuenre	Kysen	Maakha
Mahu	Mahuhy	Maiherpri
Ma'nakhtuf	Manetho	Masaharta
May	Maya	Mehy
Meketre	Mekhu	Men
Menkheperraseneb	Menkheperre	Menmet-Ra
Menna	Mentuemhat	Mentuherkhepshef
Meremptor	Merenamun	Merenkhons
Merenptah	Mereruka	Merka
Mernebptah	Mery	Meryamun
Meryatum	Meryawy	Merymose
Meryptah	Meryrahashtef	Meryre
Mes	Min	Minkhat
Minmose	Minnakht	Mokhtar
Montjuemhat	Montjuhirkopshef	Montuemhet
Mose	Naga-ed-der	Nakhthorheb
Nakhtimenwast	Nakhtmin	Nakhtnebef
Naneferkeptah	Nebamun	Nebankh
Nebemakst	Nebhotep	Nebimes
Nebitka	Nebmaetre	Nebnefer
Nebnetjeru	Nebseni	Nebseny
Nebwennenef	Nechoutes	Neferhotep
Neferhotpe	Neferkheperuhersekheper	Nefermaet
Nefermenu	Neferrenpet	Neferti
Nehasy	Nehi	Nekau
Nekhwemmut	Nendjbaendjed	Nenedjebaendjed
Neneferkaptah	Nenkhefta	Nes
Nesamun	Neshi	Neshorpakhered
Neskhons	Nesmont	Nespaherenhat
Nespakashuty	Nespatytawy	Nespherenhat
Nessuimenopet	Nestanebetasheru	Nestefnut
Netihur	Nigmed	Nimlot
Niumateped	Pabasa	Pabernefy
Padiamenet	Padiamenipet	Padiamun
Padineith	Paheripedjet	Pairy
Pait	Pakharu	Pakhneter
Pamont	Pamose	Pamu
Panas	Paneb	Paneferher
Panehesy	Paperpa	Paramesse
Parennefer	Pasebakhaenniut	Pasekhonsu
Paser	Pashedbast	Pashedu
Pasherdjehuty	Pa-Siamun	Pawiaeadja
Paynedjem	Payneferher	Pediamun
Pediese	Pedihor	Penamun
Penbuy	Penmaat	Pennestawy
Pentaweret	Pentu	Pepynakhte
Peraha	Pinhasy	Pinotmou
Prahotpe	Pramessu	Preherwenemef
Prehirwennef	Prepayit	Psamtek
Psenamy	Psenmin	Ptahhemakhet
Ptahhemhat-Ty	Ptahhotep	Ptahhudjankhef
Ptahmose	Ptahshepses	Qenymin
Rahotep	Rahotpe	Raia
Ramessenakhte	Ramessu	Rekhmire
Reuser	Rewer	Roma-Roy
Rudamun	Sabef	Sabni
Salatis	Samut	Sanehet
Sasobek	Sawesit	Scepter
Sekhemkare	Sekhmire	Seneb
Senebtyfy	Senemut	Senmen
Sennedjem	Sennefer	Sennufer
Senui	Senwosret	Serapion
Sese	Setau	Setep
Sethe	Sethherwenemef	Sethhirkopshef
Sethnakhte	Sethnakte	Sethy
Setne	Setymerenptah	Shedsunefertum
Shemay	Shepenwepet	Siamun
Siese	Si-Mut	Sinuhe
Sipair	Sneferu	Somtutefnakhte
Surero	Suty	Sutymose
Takairnayu	Takany	Tasetmerydjehuty
Tayenimu	Tefibi	Tenermentu
Teti-en	Tetisheri	Tjaenhebyu
Tjahapimu	Tjaroy	Tjauemdi
Tjenna	Tjety	To
Tui	Tutu	Tymisba
Udjahorresne	Udjahorresneith	Uni
Userhet	Usermontju	Wadjmose
Wahibre-Teni	Wahka	Webaoner
Webensenu	Wedjakhons	Wenamun
Wendjabaendjed	Wendjebaendjed	Weni
Wennefer	Wennufer	Wepmose
Wepwawetmose	Werdiamenniut	Werirenptah
Yanhamu	Yey	Yii
Yuya	Zazamoukh"""

let mefemale = """Inanna	Pu-abi
Silili
Ayzebel	Dido	Jezebel
Similce	Sophoniba	Sophonisba
Sophonsiba	Yzebel
Aishah	Hind	Maesa
Mamaea	Mawia	Sammu-ramat
Samsi	Semiramis	Tanit
Urshanabi	Zabibi	Zainab
Zebba	Zenobia
Pudukhepa
Abana	Adjedaa	Ahwere
Amenirdis	Amenkhenwast	Amosis
Anhay	Ankhesenamun	Ankhesenaten
Ankhesenneferibre	Ankhetperure	Ankhnesmeryre
Ankhnesneferibre	Asenath	Baktre
Baktwerel	Beketaten	Bithiah
Duathor	Esemkhebe	Hehenhit
Hentempet	Henttimehu	Henut
Henutmire	Hetepheres	Hrere
Huy	In	Inhapi
Inihue	Ipip	Ipu
Ipuky	Ipy	Iras
Isetemkheb	Isetnefret	Isiemkheb
Iuhetibu	Karpes	Kawit
Kem	Khedebneithireretbeneret	Khutenptah
Maatneferure	Maetkare	Maharet
Makare	Mayati	Mehetweshkhet
Mehtetweshket	Mehytenweskhet	Meketaten
Meketre	Mekhare	Meresankh
Meritaten	Meryetamun	Meryetaten
Meryetre	Meryt	Mutemhab
Naneferher	Nany	Naunakhte
Nebefer	Nebnofret	Neferet
Neferhetetepes	Neferneferuaten	Neferneferure
Nefertiry	Neferu	Neferubity
Neferuptah	Neferure	Neferusherit
Neskhons	Nestanebtishru	Nitetis
Nitiqret	Niutnakht	Nodjmet
Nofretiri	Nofritari	Nofrure
Nubkhaes	Nubkhas	Nyla
Rai	Reddjedet	Reonet
Roy	Ruia	Satdjehuty
Satnebetneninesu	Sebtitis	Senebtisi
Senebtysy	Senet	Senmonthis
Sennuwy	Sentnay	Shesh
Sit-Hathor-Iunet	Sitkamose	Sitre
Sobekemshaf	Sotepenre	Sponnesis
Tabes	Tabesheribet	Tabubu
Taheret	Tahpenes	Tairetdjeret
Tais	Taiuhery	Takhat
Tamin	Tanetnephthys	Taweret
Tayuheret	Tetisherit	Tiaa
Timat	Tjia	Tjuiu
Tjuyu	Tutu	Wenis
Weret	Wernero"""

let english =
    ["""Adam	Geoffrey	Gilbert
Henry	Hugh	John
Nicholas	Peter	Ralf
Richard	Robert	Roger
Simon	Thomas	Walter
William""";
"""Agnes	Alice	Avice
Beatrice	Cecily	Emma
Isabella	Joan	Juliana
Margery	Matilda	Rohesia""";
"""Achard
Addinell	Adeney	Aguilon
Albelin	Alevi	Alis
Altard	Ansgot	Anzeray
Arundel	Aschuill	Asselin
Auber	Aubert	Auffrye
Aungier	Auvray	Azor
Bachiler	Baignard	Bailleul
Bainard	Baliol	Ballard
Barkentin	Basnage	Basset
Baudry	Baujot	Bauldry
Bauquemare	Bavent	Beaumanoir
Beaumarchais	Beaumont	Beauvallet
Becdelièvre	Bele	Belet
Bellecote	Belmis	Benoist
Beringar	Berners	Bernières
Bertran	Bigot	Blancbaston
Blangi	Blosbeville	Blouet
Bohon	Boisivon	Boislevesque
Boissel	Boivin	Bolam
Bolbec	Bondeville	Bonel
Bonenffant	Boneth	Bonvalet
Bordel	Bosanquet	Bosc
Bosiet	Bossard	Bostel
Boteler	Boterel	Botin
Bouchard	Bourchier	Bourdekin
Bourdet	Bourneville	Bradwardine
Brai	Braund	Brebeuf
Brereton	Bretel	Breteuil
Bretteville	Brèvedent	Brimou
Brinon	Briouse	Briqueville
Brix	Buci	Budi
Bulli	Burci	Burguet
Buron	Bursigni	Busnois
Busquent	Caen	Cailli
Caillot	Cairon	Calmette
Cambrai	Campion	Canaigres
Canouville	Caradas	Carbonnel
Cardon	Cardonell	Carnet
Carteret	Castillon	Caunter
Cavelier	Ceauce	Cely
Challenge	Chandos	Chartres
Chauncy	Cheney	Cherbourg
Cioches	Claville	Clerinell
Clinchamps	Coliar	Colleville
Colombelles	Colombieres	Comyn
Conteville	Corbet	Corbière
Corbon	Cormeilles	Corneilles
Corviser	Cosin	Couci
Couer	Courcelle	Courcelles
Courci	Courcon	Courcy
Courseume	Craon	Crevecoeur
Croc	Cruel	Cugey
Cul de Louf	Culai	Cumin
Curteys	d'Ouilli	d'Adreci
d'Aguillon	d'Albert	d'Alencon
Dalyngridge	d'Amboise	d'Ambray
Damours	d'Andeli	d'Andre
d'Angers	d'Angerville	d'Angleville
Danneville	d'Ansleville	Danvers
Darcy	Darell	d'Argentan
D'Argouges	d'Argues	d'Armentieres
d'Arques	d'Athenous	d'Aubernon
d'Auberville	d'Audrieu	d'Aufai
d'Aumale	Daunger	d'Aunon
D'Auvay	D'Auvrecher	d'Avranches
d'Avre	de Bailleul	de Balon
de Bans	de Bapaumes	de Barbes
de Beauchamp	de Beaufou	de Beaumais
de Beaumont	de Beauvais	de Bellehache
de Bellemare	de Bellièvre	de Belmeis
De Berchelai	de Bercheres	de Bernai
de Bernieres	de Berranger	de Berville
de Bethencourt	de Bienfaite	de Biville
de Blays	de Blundville	de Bouilon
de Bourgueville	de Breos	de Cahaihnes
de Calmesnil	de Caulmont	de Caux
de Challon	de Chefderue	de Civille
de Corbeil	de Cormeilles	de Coucy
de Courseilles	de Croismare	de Faicterau
De Felius	De Fry	de Genville
de Gosbeck	de Grieu	de Hanivel
de Hattes	de Herle	de Ireby
de La Champagne	de La Hay	de La Mare
de La Noue	de La Place	de La Porte
de La Reue	de La Roche	de Lampérière
de Lombelon	de Lorraine	de Malhortye
de Maromme	de Massard	de Mesniel
de Mesnildo	de Monchy	de Monluc
de Montchrestien	de Montfault	de Montgomery
de Moustiers	de Moy	de Munchesney
de Pardieu	de Perronet	de Pinchemont
de Recusson	de Rely	de Reymes
de Roncherolles	de Salynges	de Saussay
de Savage	de Seguzzo	de Servian
de Seyssel	de Tanie	de Tocni
de Toeni	de Valles	de Vandes
de Vaux	de Vesci	de Villy
de Viuepont	de Vymont	d'Ecouis
d'Engagne	d'Eresby	des Moutiers
des Vaux	d'Escalles	Deschamps
Desmares	d'Espagne	Destain
d'Eu	d'Evreux	d'Helion
d'Hericy	d'Houdetot	Digby
d'Incourt	Ditton	Dive Beugelin
d'Ivri	Dol Hugue	d'Olgeanc
d'Orbec	d'Orglande	d'Ornontville
Douai	Dreux	Droullin
Druel	du Bec	du Bois-Hebert
du Bosc-Roard	du Breuil	Du Buisson
Du Gouey	du Merle	Du Moucel
du Perche	Du Perron	du Quesnai
du Saussai	du Theil	du Tilleul
Dubosc	Dufay	Dufour
Duhamel	Dumont	d'Unepac
Dupasquier	Duquesne	Durandal
Durerie	Durjardin	Durville
Duval	Dyel	Ecouland
Elers	Emory	Engerrand
Erquemboure	Espec	Esteney
Evelyn	Eveque	Faceby
Faintree	Falaise	Fantosme
Faucon	Fecamp	Fergant
Ferrieres	Feu	Fitzalan
Fitzherbert	Fitzhugh	Fitzroy
Flambard	Folet	Foliot
Fonnereau	Fontemai	Fossard
Fougeres	Fourneaux	Framan
Fresle	Fribois	Froissart
Fromentin	Furnival	Gael
Gand	Garin	Gaveston
Gibard	Giffard	Gillain
Gilpin	Giscard	Glanville
Godart	Godefroy	Gomboult
Gouel	Goulaffre	Gournai
Grai	Grancourt	Grentemesnil
Grenteville	Greslet	Griffin
Grimoult	Grouchet	Groulart
Guéribout	Guernon	Gueron
Guideville	Guiffart	Guildersleeve
Guinand	Gurney	Guyot
Hachet	Halacre	Hallé
Hamage	Harcourt	Haute
Hauville	Hédiart	Hendry
Herbard	Heriet	Heuzé
Hewse	Hodenc	Holland
Hotot	Hue	Hugonin
Hynde	Ide	Jolland
Jubert	la Berviere	la Bruiere
la Cleve	la Foret	la Guierche
la Mare	la Pommeraie	la Riviere
La Vache	La Verrier	Labbé
Laci	l'Adoube	l'Aigle
Lallement	l'Ane	Lanquetot
l'Appeville	l'Archer	l'Aune
Le Barge	le Berruier	Le Blanc
le Blond	le Bouguignon	le Breton
Le Chandelier	Le Clerc	Le Conte
Le Cordier	Le Cornu	le Despensier
Le Doulcet	le Flamand	le Gaucher
Le Goix	Le Grant	Le Gras
Le Jumel	Le Lieur	Le Maistre
Le Marchant	le Marechal	Le Marinier
Le Masson	Le Paulmier	Le Pelletier
Le Pesant	le Poitevin	Le Prévost
le Roux	Le Roux	Le Seigneur
le Senechal	Le Sueur	Le Tellier
le Vicomte	Lefebre	l'Estourmi
Letre	Levasseur	Lhuillier
Libourg	Ligonier	L'ile
Linesi	Lisieux	Loges
Lorz	Loucelles	Louet
Louvet	Lovet	Lucy
Ludel	Lynom	Machault
Machel	Maci	Maignart
Malet	Mallebisse	Malleville
Mallilie	Mallory	Malvallet
Malveisin	Maminot	Mandeville
Manneilli	Mansel	Mantel
Marchés	Marchmain	Marci
Marescot	Margas	Mariage
Marillac	Marisco	Martel
Mathan	Maubenc	Maudit
Mauduit	Maunsell	Maurouard
Mautravers	Maynet	Medley
Mercier	Meri	Merteberge
Mesnage	Meulan	Meules
Meverel	Middleton	Mobec
Moion	Monceaux	Montacute
Montaigu	Montbrai	Mont-Canisi
Montfiquet	Montfort	Montgomery
Montgommeri	Moron	Morphew
Mortagne	Mortain	Mortemer
Mortmain	Moyaux	Mucedent
Munneville	Murdac	Musard
Musart	Mussegros	Mustel
Nelond	Neot	Nesdin
Neufmarche	Neuville	Noyers
Omand	Orlebar	Ormond
Osmond	Osmont	Ouistreham
Painel	Paixdecouer	Pancevolt
Pantoul	Papelion	Papon
Paris	Parry	Parthenai
Paschal	Pasquier	Pastforeire
Patris	Paumera	Peccoth
Peche	Peis	Pennant
Perci	Péricard	Perroy
Petremol	Peveril	Pevrel
Picard	Picot	Picvini
Pierrepont	Pinel	Pipin
Pippery	Piquiri	Pistres
Pithou	Plucknet	Poer
Poignant	Poillei	Pointel
Pont	Pont de l'Arche	Pontchardon
Port	Postel	Poussin
Prestcote	Puchot	Quesnel
Quièvremont	Quincarnon	Raimbeaucourt
Rainecourt	Raleigh	Rames
Raoullin	Rassent	Ravenot
Rennes	Renold	Restault
Reviers	Riebou	Rivière
Roard	Rocque	Roger
Romé	Romenel	Ros
Rosai	Rou	Roussel
Runeville	Sacquerville	Saint-Clair
Sainte-d'Aignaux	Saint-Germain	Saint-Helene
Saint-Leger	Saint-Ouen	Saint-Quentin
Saint-Sanson	Saint-Valeri	Saint-Waleri
Saisset	Sauvigni	Scolland
Séguier	Senarpont	Senlis
Sept-Meules	Simnel	Sollers
Somneri	Sourdeval	Strivelyn
Stukely	Tabraham	Taillebois
Taillepied	Talvace	Tessel
Thaon	Theroulde	Thibault
Thiboust	Thorel	Tibon
Tilly	Tinel	Tirel
Toclive	Torteval	Touchet
Tourlaville	Tourmente	Tournai
Tournebulle	Tournebut	Tourneville
Toustain	Tranchant	Trelli
Tulles	Urry	Valance
Valognes	Vastel	Vatteville
Vaubadon	Vauville	Vaux
Vavassour	Veci	Venois
Ventris	Ver	Verdun
Vernold	Vernon	Verrall
Vesci	Vesli	Veteripont
Vieuxpont	Villehardain	Villon
Vipont	Vis-de-Louf	Vis-de-Loup
Vitalis	Vivers	Viville
Voisin	Wadard	Warci
Watteau	Werables	Willoughby
Wissant	Ygou
Beauchamp
Bigod	Bohun	Boleyn
Cecil	Courteney	Dacre
de Braose	de Burgh	de Clare
de la Pole	de Lacy	de Montfort
Devereux	Dudley	Fairfax
Ferrers	Fiennes	Fitton
Herbert	Howard	Lovell
Montague	Mortimer	Mowbray
Neville	Percy	Quincey
Russell	Sidney	Stafford
Stanley	Talbot	Umfraville
Vane	Vaughan	Woodville"""
    ]

let greece =
    ["""Abantes	Abas	Abascantus	Abderos	Aberkios	Ablerus
Abreas	Abronychus	Abydos	Acaeus	Acamus	Acessamenus
Acestes	Aclepiades	Acrisias	Acrisius	Acroneos	Actor
Adeimanthos	Adelphius	Admetos	Adrastos	Adrastus	Adrestus
Aeaces	Aegaeon	Aegicoros	Aegisthes	Aegon	Aeimnestos
Aenesidemos	Aeolus	Aeropus	Aeschreas	Aesculapius	Aesepus
Aeson	Aesop	Aetes	Aethon	Aetion	Aetios
Aetolos	Agamedes	Agamemnon	Agapenor	Agapias	Agastrophos
Agathocles	Agathon	Agelaus	Agenor	Agesilaus	Agetos
Agis	Agrias	Aiantes	Aias	Aigeus	Airopos
Aischylos	Akadios	Akamas	Aktis	Aktor	Alastor
Alcaeos	Alcandros	Alcides	Alcimos	Alcinous	Alcmaion
Alcman	Alcon	Alector	Alektryon	Aleuas	Alexandros
Alexarchos	Alexias	Alexis	Alexon	Alkamenos	Alkestis
Alketas	Alkibiades	Alkides	Alkimachos	Alkiphron	Alkmaion
Aloeus	Alphaeus	Alpheos	Alphesiboeus	Alphios	Altes
Alypius	Amarinceus	Ameinias	Ameinokles	Amiantos	Amompharetos
Amopaon	Amphiaraos	Amphidamos	Amphimachos	Amphimnestus	Amphinomous
Amphion	Amphios	Amphitrion	Amyntas	Amyntor	Amyris
Amythaon	Anabesineos	Anacharsis	Anakletos	Anakreon	Anastasios
Anaxagoras	Anaxandridas	Anaxandrides	Anaxandros	Anaxarchos	Anaxilaus
Anaximander	Anaximenes	Anaxis	Anaxos	Anchialus	Anchimolios
Anchises	Ancus	Andokides	Andraemon	Andreas	Androbulos
Androcles	Androdamos	Androgeus	Aneristos	Aniketos	Anisodoros
Antaeus	Antagoras	Antemion	Antenor	Anthemion	Antichares
Antidoros	Antigenes	Antigonos	Antikles	Antilochus	Antinous
Antiochus	Antipatris	Antipatros	Antiphales	Antiphones	Antiphus
Antisthenes	Anytos	Anytus	Apelles	Apellicon	Aphidnos
Apisaon	Apollodoros	Apollophanes	Apollos	Aratus	Arcas
Arcesilaus	Archagoras	Archelaos	Archeptolemus	Archesilaus	Archestratidas
Archilochus	Archytas	Arcidamus	Arcturus	Areilycus	Areisius
Areithous	Argades	Argaeus	Argos	Aridolis	Arion
Ariphron	Aristaeus	Aristagoras	Aristaios	Aristandros	Aristarchos
Aristarchus	Aristides	Aristion	Aristippus	Aristoboulos	Aristobulus
Aristocles	Aristocypros	Aristodemos	Aristogeiton	Aristomachos	Ariston
Aristonous	Aristonymos	Aristophanes	Aristophantes	Aristos	Aristotles
Aristoxenus	Arrabaios	Arridaios	Arsenios	Artemas	Artemidoros
Artemios	Artemisthenes	Arybbas	Asasthenes	Ascalaphus	Ascanius
Aschines	Asius	Asklepios	Asonides	Asopodoros	Asopus
Asphalion	Assaraeus	Astacos	Aster	Asterion	Asteropaeus
Astrabacus	Astyanax	Athamas	Athenades	Athenaeus	Athenion
Athenodorus	Atiphates	Atreus	Atrometos	Attaginas	Attaginos
Attalos	Atymnius	Atys	Audax	Augias	Auletes
Autesion	Autodikos	Autolycus	Autolykos	Automedon	Autonous
Axylus	Azeus	Bacchides	Bacchios	Bacchylides	Bacenor
Bacis	Baerius	Balius	Barates	Bardas	Basileides
Basileios	Basilides	Bathyaes	Belos	Bendis	Bianor
Bias	Bion	Bisaltes	Biton	Blathyllos	Boethus
Borus	Boter	Briareus	Briarus	Brison	Brygos
Bucoli	Bulis	Burrhus	Butacidas	Callimachus	Callimorphus
Carenos	Carneades	Carpophorus	Carpus	Casambus	Castor
Ceas	Cebriones	Celeas	Cephalos	Cepheus	Cephissos
Ceyx	Chabrias	Chaeremon	Chairophon	Chalcodon	Chalcon
Charax	Chares	Charidemos	Charilaus	Charillos	Charmides
Charon	Charopos	Cheiron	Chersis	Chileos	Chilon
Choerilos	Choeros	Chremes	Chremon	Chremonides	Chromis
Chromius	Chrysaor	Chryses	Chrysippos	Chrysogones	Chrysogonus
Chrysolorus	Cilix	Cineas	Cinyras	Cisses	Cisseus
Cleades	Cleandros	Cleathes	Cleisthenes	Cleobulus	Cleodaeos
Cleombrotos	Cleomenes	Cleon	Cleonicus	Cleonymus	Clinias
Clisthenes	Clonius	Clytius	Clytomedes	Cnoethos	Cobon
Codros	Coenus	Coeranus	Coes	Cois	Conon
Cöon	Copreus	Cordylion	Coronos	Corydallos	Corydon
Crathis	Cratinus	Cratippus	Cretheus	Crethon	Cretines
Crios	Croesus	Cronos	Cteatus	Ctesippus	Cuphagoras
Cyberniskos	Cycnus	Cylon	Cynaegiros	Cyncus	Cyneas
Cyniscus	Cypselos	Cyrenios	Cytorissos	Dadaces	Daedalos
Daetor	Damasippus	Damasithymos	Damasos	Damastor	Damian
Damianos	Damiskos	Damoetas	Damon	Danaos	Danaus
Daphis	Daphnis	Dardanus	Dares	Davos	Deinias
Deinokrates	Deinomenes	Deiotones	Deiphobus	Deiphonous	Deipylus
Demades	Demaratos	Demarmenos	Demas	Demeas	Demetrios
Democedes	Democoön	Demodocus	Demokrates	Demoleon	Demonax
Demonous	Demophlos	Demosthenes	Deon	Derkylos	Deukalion
Dexicos	Dexios	Diactorides	Diadromes	Diadumenus	Diagoras
Dicaeus	Dieneces	Diocles	Diodoros	Diodorus	Diokles
Diomedes	Dionysios	Dionysophanes	Dionysos	Diophantus	Diores
Dioscuros	Diotrephes	Dismas	Dithyrambos	Dmetor	Dolon
Dolops	Doreios	Doreius	Dorian	Doriskos	Doros
Dorotheus	Doryssos	Dosithios	Drimylos	Dromeus	Dryas
Dryops	Ducetius	Duris	Dymas	Dymnos	Echëeus
Echekrates	Echelaos	Echemmon	Echemus	Echephron	Echepolus
Echestratos	Eetion	Eioneus	Eirenaios	Elasus	Elatos
Elatreus	Eleon	Elephenor	Elpenor	Elpides	Elpidius
Empedocles	Endios	Endymion	Engenes	Eniopus	Ennaeus
Ennomus	Ennychus	Enops	Eos	Epaenetus	Epaphos
Epaphroditus	Epeigeus	Epeius	Ephialtes	Epicurus	Epicydes
Epikrates	Epimenes	Epiphanes	Epistor	Epistrophos	Epitrophos
Epizelos	Erasistratus	Eratosthenes	Eratostheres	Erechtheus	Eretmenus
Ereuthalion	Erginus	Ergiyios	Erichthonius	Erxandros	Eryalus
Erysichton	Eryx	Eryximachos	Eteocles	Eteokles	Eteonous
Euaemon	Eualcidas	Euanthes	Euarestos	Eubalus	Eubulus
Eucarpus	Euchenor	Eucleides	Eudorus	Eudoxsus	Eudoxus
Euenius	Euenor	Euenus	Eugammon	Eugenios	Eugenius
Euhemenis	Euippus	Eukles	Eumaeus	Eumastas	Eumelus
Eumenes	Eumneus	Eumolpus	Euneas	Euonomos	Eupalinus
Euphenes	Euphorbos	Euphorion	Euphronios	Eupolos	Euripides
Euryanax	Eurybates	Eurybiades	Eurycliedes	Eurydamus	Eurydemon
Eurydemos	Euryhus	Eurykrates	Eurykratides	Euryleon	Eurylochos
Eurymachos	Euryphon	Eurypylos	Eurystenes	Eurysthenes	Eurystheus
Eurysthios	Eurythion	Eurytos	Eussorus	Euthydemos	Euthynos
Eutropios	Eutuches	Eutychides	Eutychus	Evaenetos	Evagoras
Evandros	Evanetus	Evelthon	Evenios	Evenus	Evios
Exaduis	Exekias	Faenus	Galenus	Gallus	Ganymedes
Gauanes	Geleon	Gelo	Gelon	Gennadios	Gerasimos
Giorgius	Glaukias	Glaukos	Glycon	Gnipho	Gordias
Gorgias	Gorgion	Gorgos	Gorgythion	Gregorius	Gryllus
Gurgos	Gylippos	Gyras	Gyrtias	Haemon	Hagias
Hagnon	Halisthertes	Halius	Harmatidas	Harmocydes	Harmodios
Harmon	Harpagos	Harpalion	Harpalos	Harpocras	Hecataeus
Hegesandros	Hegesistratos	Hegetoridas	Heirax	Heiron	Hektor
Helenos	Helgesippos	Helicaon	Heliodorus	Helios	Helle
Hephaestos	Herakleides	Herakleitos	Heraklides	Hermeias	Hermeros
Hermippos	Hermogenes	Hermolaos	Hermolycus	Hermon	Hermotimos
Hero	Herodes	Herodianus	Herodion	Heromenes	Hicetaon
Hiero	Hieronymus	Hipparchos	Hipparinos	Hippasus	Hippias
Hippocoön	Hippoklides	Hippokratides	Hippolytos	Hippomachos	Hippomenes
Hippon	Hipponax	Hipponicus	Hipponous	Hippotas	Hippothous
Hippotion	Hoiples	Homeros	Hyakinthos	Hylas	Hyllos
Hyllus	Hypatius	Hypeirochus	Hypenor	Hyperenor	Hyperion
Hypsenor	Hyrcanus	Hyrtacus	Hyrtius	Iakchos	Ialmenes
Iambulus	Iamus	Iasos	Iatragoras	Iatrokles	Ibanolis
Ibykos	Icarion	Icarius	Icarus	Idaeus	Idaios
Idas	Idomeneus	Ilioneus	Illyrius	Ilus	Imbrasus
Imbrius	Imbrus	Inachos	Inachus	Inaros	Iobates
Iolaos	Iollas	Ion	Iphiclus	Iphicrates	Iphikrates
Iphinous	Iphitos	Iphitus	Iros	Irus	Isagoras
Isandros	Ischenous	Isidor	Isidoros	Ision	Ismaros
Ismenios	Isocrates	Isodemos	Isokrates	Itheus	Itylus
Itys	Kadmos	Kaenas	Kaeneus	Kalchas	Kalesius
Kaletor	Kalliaros	Kallias	Kallikles	Kallikrates	Kallimachos
Kallinicus	Kallinos	Kallipides	Kallipos	Kallisthenes	Kallon
Kameirus	Kandaules	Kannadis	Kapaneus	Kapys	Karipos
Karopophores	Kasos	Kassandros	Kaunos	Kebalinos	Kebes
Kekrops	Keos	Kephalon	Kephalos	Kerameikos	Kerkyon
Keteus	Kimon	Kirphis	Kittos	Kleitos	Kleobis
Kleomenes	Koines	Koinos	Konon	Koragos	Korax
Kosmas	Krantor	Krateros	Kreon	Krinippos	Kristos
Kritias	Kritoboulos	Kritodemos	Kriton	Kroisos	Krokinos
Ktesiphon	Kyknos	Kynaegeiros	Kyrillos	Kyrios	Kyros
Labdacus	Labotas	Laertes	Lagos	Laios	Lamachos
Lampo	Lampon	Lampus	Lamus	Laodamas	Laodocus
Laogonus	Laomedon	Laphanes	Lasos	Lasthenes	Laureion
Leagros	Leandros	Learchos	Leicritus	Leitus	Lemnus
Leo	Leocedes	Leodes	Leon	Leonidas	Leonnatos
Leontiades	Leontis	Leoprepes	Leotychides	Lethos	Leucippus
Leukos	Lichas	Licymnios	Linus	Loxias	Lukos
Lycaon	Lycaretos	Lycidas	Lycomedes	Lycophon	Lycophron
Lycoris	Lycurgos	Lycus	Lydus	Lygdamis	Lykomedes
Lykon	Lynceus	Lysagoras	Lysandros	Lysanios	Lysias
Lysikles	Lysimachos	Lysippos	Lysippus	Lysis	Macar
Macarias	Machaon	Maeon	Maiandrios	Makarios	Maleos
Males	Mantes	Mantios	Marcion	Marnes	Maro
Maron	Marsyas	Mastor	Matullus	Mausolos	Mecistes
Mecistios	Medios	Medon	Medus	Megadates	Megakles
Megakreon	Megapenthes	Megareus	Megasthenes	Megathenes	Meges
Megistias	Meidias	Melampos	Melampus	Melanippos	Melanthios
Melanthos	Melas	Meleagros	Melegros	Meles	Meliboeus
Melicertes	Memnon	Menalcas	Menandros	Menares	Menekrates
Menelaos	Menestas	Menesthes	Menesthios	Menexinos	Menoeces
Menoitios	Mentes	Mentor	Meriones	Mermerus	Merops
Mesaulius	Mesthles	Methodios	Metiochus	Meton	Metrobius
Metron	Metrophanes	Meurius	Micythos	Midas	Midylos
Mikkos	Mikon	Milanion	Miltiades	Minos	Misenus
Mnasyllus	Mnesiphilos	Mnester	Mnesus	Moeris	Moliones
Molpagoras	Monoecus	Monomachus	Mopsius	Mopsus	Morsimus
Morys	Moschion	Mulius	Musaeus	Musaios	Mydon
Mygdon	Myrsinus	Myrto	Mys	Narkissos	Nastes
Naubolus	Naukles	Nausithous	Nauteus	Nearchos	Neleos
Nelpus	Neokles	Neoptolemos	Neritos	Nestor	Niarchos
Nicandros	Nicanor	Nicholas	Nicholaus	Nicias	Nicodromos
Nicolaus	Nicomachos	Nicon	Nikandros	Nikanor	Nikasios
Nikeratos	Nikias	Nikomachos	Nikomedes	Nilus	Nireus
Nisos	Noemon	Nomion	Nothon	Numa	Nyctinus
Nymphicus	Nymphodorus	Ocealus	Ochesius	Ochos	Ocytos
Odaenathus	Odius	Odysseus	Oeagnus	Oecleus	Oedipus
Oenemaus	Oeneus	Oenomaus	Oenopion	Oenops	Oicles
Oileas	Oliatos	Olus	Olympicus	Olympiodorus	Onamakritos
Onesilos	Onesimos	Onesiphorus	Onetas	Onetor	Onias
Onomastos	Ophelestes	Opites	Ops	Orcus	Orestes
Oresus	Orges	Oribasius	Orion	Orius	Oroites
Orpheus	Orsilochus	Orsiphantes	Orthaeus	Orythroneus	Otreus
Otrynteus	Otus	Paeëon	Paios	Palaechthon	Palaemon
Pallans	Pallas	Palmys	Pammon	Panaetios	Panaetius
Panares	Pandaros	Pandion	Panionos	Panites	Pankratios
Pantares	Panthous	Pantites	Paopeus	Paraebates	Paris
Parmenides	Parmenion	Parthenopaeus	Pasion	Pataicos	Patrobas
Patrobus	Patroclus	Patron	Pausanius	Pedaeus	Pedasus
Pedocles	Peirithous	Peiros	Peisandros	Peithon	Pelagon
Pelegon	Peleus	Pelias	Pelicles	Pelonus	Pelopidas
Peneleos	Peneus	Pentheus	Penthylos	Peolpidas	Perdikkas
Perdix	Periandros	Periclymenus	Perieeres	Perikles	Perimedes
Perimos	Periphas	Periphetes	Periscus	Peritas	Periumus
Peteos	Peukestes	Phaedo	Phaenippos	Phaeops	Phaestus
Phaidon	Phaidriades	Phalanthus	Phalces	Phalinos	Phanagoras
Phancis	Phanes	Phanias	Phantias	Pharnaces	Phausius
Phegeus	Pheidias	Pheidippides	Pheidon	Phemius	Phereclus
Pherecydes	Pheres	Pheronactus	Phidias	Phigaleios	Philagros
Philaon	Phileas	Philemon	Philetor	Philiskos	Philistos
Phillipos	Philocion	Philocrates	Philoctetes	Philocypros	Philoetius
Philogus	Philokles	Philokrates	Philolaos	Philologus	Philomen
Philomenes	Philometer	Philon	Philonikos	Philopoemon	Philostratos
Philostratus	Philotas	Philotectes	Philoxenos	Philpoemon	Phineus
Phintias	Phlaris	Phlegon	Phlios	Phoenix	Phoibus
Phoinix	Phoitios	Phokas	Phokion	Phorbas	Phorcys
Phormion	Phormos	Photius	Phrixus	Phrynichos	Phrynikos
Phrynon	Phylacus	Phylas	Pidytes	Pigres	Pinder
Pirithoos	Pisistratos	Pistias	Pittacos	Pittacus	Pittheus
Pixodarus	Plades	Pleistarchos	Pleistos	Plutarch	Podaeleirus
Podaleirus	Podalinus	Podarces	Podargos	Podaroes	Podes
Poeas	Poecas	Poimen	Polemion	Poliadas	Pollio
Polyas	Polybius	Polyctor	Polydectes	Polydeuces	Polydius
Polydoros	Polyeides	Polygonus	Polykleitos	Polykles	Polykritos
Polymedes	Polyneices	Polypemon	Polyperchon	Polyphemous	Polyphetes
Polyphontes	Polypoetes	Polyxeinus	Ponteus	Porphyrios	Porphyrius
Poseidon	Posides	Posidonios	Potamon	Pratinos	Praxilaus
Praxis	Praxiteles	Praxites	Prexinos	Priam	Prinetadas
Priskos	Procrustes	Proctus	Proetus	Prokles	Prokopios
Prokrustes	Proreus	Protagoras	Protesilaus	Prothoenor	Prothous
Protogenes	Protus	Proxenos	Prymneus	Prytanis	Ptolemaios
Ptolomaeus	Pylades	Pylaemenes	Pylaeus	Pylartes	Pylas
Pylenor	Pyris	Pyrrhus	Pythagoras	Pytheas	Pythes
Pythios	Pythogenes	Radamanthos	Rhadamanthos	Rhesus	Rhexenor
Ribes	Rizon	Sabas	Sabyllos	Salmoneus	Sarpedon
Satyros	Scaios	Scamandius	Scamandrius	Schedius	Scylax
Scyllias	Scythas	Sebastos	Seisames	Selagus	Seldomus
Selepos	Seleukos	Sicinnos	Siculus	Silanos	Silenos
Simmias	Simo	Simoisius	Simonides	Sinis	Sinon
Sippas	Siromos	Sisyphus	Skiron	Smindyrides	Smintheus
Socus	Sophanes	Sophokles	Soranus	Sosibios	Sosicles
Sosigines	Sosilus	Sosimenes	Sosipatros	Sosthenes	Sostias
Sostratos	Spertias	Speudon	Speusippos	Spinther	Spirodion
Stachys	Stentor	Stesagoras	Stesanor	Stesilaus	Sthenelaus
Sthenelus	Stichius	Stolos	Strabo	Strachys	Stratios
Straton	Strophantes	Strophius	Strymon	Syagros	Syennesis
Syloson	Synesius	Talaemenes	Talaos	Talaus	Talos
Talthybios	Tarchon	Taureas	Tebaeus	Tecton	Teiresias
Telamon	Telekles	Telemacho	Telemachos	Telemachus	Telephos
Telephus	Telesinus	Telesphorus	Telines	Tellias	Tellis
Telys	Temenos	Tenes	Tenes	Tenthredon	Tereus
Terillos	Teucer	Teukros	Teutamos	Teuthranes	Teuthras
Thales	Thalpius	Thalysios	Tharybis	Thaulos	Thaumastus
Theagenes	Theages	Theas	Theasides	Themistius	Theoclymnius
Theocydes	Theodekles	Theodoros	Theodotus	Theognis	Theomestor
Theomestros	Theophanes	Theophrastos	Theophrastus	Theophylaktos	Theopompos
Theopompus	Theopropides	Theoros	Theos	Theramenes	Therapon
Theras	Thero	Theron	Thersandros	Therseandros	Thersilochus
Thersites	Thessalos	Thestor	Thettalos	Thoas	Thon
Thoön	Thorax	Thrasidaios	Thrasilaus	Thrasius	Thrasybulos
Thrasyllus	Thrasymedes	Threspotus	Thukydides	Thyestes	Thymoetes
Thymotes	Thyrsis	Thyrsos	Timagenidas	Timagoras	Timais
Timanthes	Timasion	Timasitheus	Timesithius	Timnes	Timoleon
Timon	Timonax	Timotheus	Timoxenos	Tiro	Tirynthius
Tisamenos	Tisandros	Tisias	Tithonius	Titormos	Tityrus
Tlepolemus	Tmolus	Trechus	Triopas	Triptolemus	Triton
Troezenus	Trophimus	Trophnus	Tros	Trypho	Turrianus
Tychaeus	Tydeides	Tydeus	Tymnes	Tyndareus	Tyndarios
Ucalegon	Vettias	Xanthippos	Xanthippus	Xanthos	Xenagoras
Xenokrates	Xenophanes	Xenophon	Xiphilinus	Xuthos	Xuthus
Zagreus	Zamolxis	Zenicetes	Zenodoros	Zephyrinus	Zethus
Zeuxidamos	Zeuxis	Zosimus""";
"""Achaia	Achradina	Actaëe	Actë	Ada	Adeia
Aedon	Aegiolea	Aegle	Aerope	Aethre	Agamede
Aganippe	Agape	Agapia	Agarista	Agathé	Agathonice
Agave	Aglaia	Aglaurus	Aikaterine	Aithra	Aketa
Alcandre	Alcestis	Alcippe	Alcmene	Alcyone	Alemene
Alkmena	Althaea	Althaia	Althea	Amarhyllis	Amathea
Amatheia	Amphithoe	Amphitrite	Ampinomene	Amplias	Anais
Anastasia	Andromeda	Antehe	Anteia	Antheia	Anthousa
Anthusa	Anticleia	Antigone	Antiochis	Antiope	Anysia
Appollonia	Apseudes	Arete	Arethusa	Argeia	Ariadne
Arisbe	Aristonike	Aristophane	Arsinoe	Artemidora	Artemisia
Aspasia	Astera	Astyoche	Astyocheia	Atalanta	Atë
Athis	Auge	Augo	Autonoe	Auxesia	Axiothea
Barbara	Basiane	Baucis	Berenike	Bito	Briseis
Caenis	Caleope	Callianeira	Callianessa	Calliphana	Calypso
Canace	Castianiera	Charis	Chione	Chiore	Chloë
Chloris	Chryse	Chryseis	Chrysothemis	Cilissa	Cilla
Circe	Clio	Clymene	Clymere	Colubra	Corythia
Cratais	Creusa	Crisa	Ctimene	Cybele	Cydippe
Cymodoce	Cymothoe	Cyrene	Cythereia	Cytheris	Damaris
Damia	Danaë	Deianeira	Deineira	Deiphobe	Deipyle
Delias	Demetria	Demophile	Dexamene	Dianeme	Diomede
Dione	Dirce	Doris	Dorothea	Doto	Drosis
Dynamene	Egeria	Egina	Eidothee	Eileithyia	Elcmene
Electra	Elpir	Endeis	Enyo	Eos	Epicaste
Eriboea	Erigone	Eriopis	Eriphyle	Eris	Eucarpia
Eudokia	Eunice	Euodias	Euphro	Euphrosyne	Europa
Eurycleia	Eurydike	Eurynome	Evadne	Galatea	Glauce
Glyke	Gorgo	Gygaea	Haidee	Halie	Harmodias
Harmonia	Hecuba	Hekabe	Hekaline	Hekate	Helice
Helike	Heliodora	Hellanike	Helle	Hermine	Hermione
Hero	Herophile	Hesione	Hilaera	Hippodameia	Hippodamia
Hippolyta	Hypsipyle	Hyrmina	Iaera	Ianeira	Ianessa
Ianthe	Ino	Iola	Iolanthe	Iole	Iomene
Ione	Iphianassa	Iphigenia	Iphimedeia	Iphis	Iphitheme
Irene	Iris	Isadora	Ismene	Issa	Jocasta
Kallisto	Kallixeina	Kassandra	Katana	Katina	Kephissa
Kharmion	Khlöe	Khloris	Kleio	Kleopatra	Klymene
Klytemnestra	Koré	Koritto	Kydilla	Kynna	Kynthia
Kypris	Kyra	Labda	Lais	Lalage	Lampetie
Lampito	Lanike	Laodameia	Laodamia	Laodice	Laothoe
Lasthena	Latona	Leda	Lede	Leto	Leucothea
Leucothoë	Limnoreia	Lois	Lyra	Maeonia	Maera
Maia	Maiandria	Marpessa	Medea	Medesicaste	Megaera
Megara	Megare	Melanie	Melantho	Melissa	Melita
Melite	Menelaia	Merope	Metis	Metriche	Milo
Milto	Molpadia	Monima	Monime	Mykale	Myrine
Nausicaa	Neaera	Nemerte	Nephele	Nesaea	Nicopolis
Nikaia	Nikasepolis	Niko	Niobe	Nysa	Oenone
Oitane	Olympias	Omphale	Oreithuia	Oreithyia	Orithyia
Orthia	Otonia	Pales	Panope	Panora	Parthenia
Parthenope	Pasiphae	Pelopia	Penelope	Penthesilea	Percalus
Perialla	Periboea	Pero	Perse	Persephone	Persis
Pervica	Pervinca	Phaedra	Phaedre	Phaethusa	Phaia
Pherenike	Pherusa	Phigaleia	Philea	Philinna	Philomache
Philomela	Philona	Phoebe	Phryne	Phylace	Phylia
Phyllis	Phylo	Phylomedusa	Podarge	Polycaste	Polydamna
Polydora	Polymede	Polyxena	Procne	Procris	Prone
Proto	Protogonia	Psamathe	Psyche	Pylia	Pyrrha
Pythias	Raisa	Rhea	Rhene	Rhoda	Rhode
Rhodope	Roxane	Sappho	Scylla	Sebasteia	Semele
Sophia	Sotera	Speio	Stheneboea	Stratonice	Tecmessa
Telephassa	Thais	Thalassa	Thaleia	Theano	Thebe
Thelma	Themis	Theodotis	Theophane	Theophania	Theophano
Theresa	Thessala	Thessalonike	Thetis	Thisbe	Thoë
Thoösa	Thyia	Timandra	Timo	Tryphena	Tryphosa
Tyro	Xanthe	Xanthippe	Xantippe	Xene	Xenophile
Zenobia	Zita	Zoe""";
]

let rome =
    [
    """Amulius	Appius (App)#	Aulus (A)	Caius (C)	Cnaeus (Cn)	Decimus (D)
Domitius	Flavus	Gaius (G)	Galerius	Gnaeus	Gneo
Julius	Kaeso	Lucius (L)	Mamercus*	Manius (M')	Marcus (M)
Numerius (N)	Olus~	Oppius	Publius (P)	Quintus (Q)	Servius (Ser)
Sextus (Sex)	Spurius (Sp)	Tiberius (Ti)	Titus (T)	Vibius
Cornelii	Gnaeus, Lucius, Publius
Julii	Gaius, Lucius, Sextus
Licinii	Lucius, Marcus, Publius
Pompeii	Gnaeus, Quintus, Sextus
Servilii	Gnaeus, Quintus""";
    """Aeliana	Aeterna	Afrania	Afrodisia	Agneta	Agrippina
Aia	Amabilis	Amalthea	Amanda	Apicata	Apphia
Apronia	Apulia	Arruntia	Astia	Atia	Augusta
Aventina	Avita	Blatta	Byrria	Caepionis	Caesonia
Calva	Camilla	Capitolina	Carina	Casta	Cata
Catia	Caula	Censorina	Cherusca	Christiana	Cinna
Clara	Cloelia	Columella	Concessa	Cordelia	Cosconia
Cotta	Coventina	Crescentia	Crispina	Cristina	Cypria
Decima	Desiderata	Dinysia	Dolichena	Domna	Dulcilla
Emelia	Emerentiana	Encratia	Ennia	Erigena	Estella
Eubala	Eubia	Eutropia	Fausta	Faustina	Faustinia
Fecenia	Felicia	Felicula	Fesonia	Flora	Fontia
Fortunata	Gaea	Gaetulica	Galla	Geneura	Gloria
Gnaea	Graecina	Grata	Hilara	Hilaria	Honora
Hypatia	Iana	Idonea	Indara	Ingenua	Innocentia
Januaria	Jovina	Juliana	Julilla	Julitta	Juncina
Justa	Justina	Lanilla	Larina	Laureola	Laurina
Lavinia	Lepida	Lesbia	Licinian	Livigena	Longina
Luciana	Lucilla	Lucina	Lupula	Lutatia	Macrina
Maesa	Maia	Mallea	Mamaea	Mamma	Mammiola
Mansueta	Mariana	Marina	Martia	Martina	Martiola
Matrona	Maxilla	Maxima	Maximilla	Messalina	Messina
Metella	Metiliana	Milvia	Myrtilla	Naissa	Narcissa
Natalia	Nobilioris	Numantina	Occia	Ocresia	Olivia
Oriuna	Pacata	Palla	Panthea	Pantheria	Pascentia
Paulla	Peregrina	Petraea	Phrygia	Pinta	Placida
Planasia	Plancina	Plautia	Plotina	Pluma	Pompeia
Poppaea	Porcella	Postimia	Potamiaena	Praxedes	Prenestina
Priscilla	Procilla	Procula	Pulchra	Pussitta	Regina
Restita	Rhea	Ria	Romana	Sabina	Sabrina
Sacrata	Salonia	Salviena	Sapientia	Saturnia	Saturnina
Scaura	Scipionis	Secunda	Selene	Selina	Serena
Severa	Severiana	Silana	Silicia	Silvia	Simplicia
Sophrona	Sosia	Sulpicia	Tacita	Tarpeia	Tebetta
Teracina	Tertia	Tertulla	Thena	Tiberia	Titullinia
Trifosa	Tristonia	Tuomina	Ulidia	Una	Ursula
Utica	Vacia	Vecchia	Velva	Velvinna	Vernico
Veronica	Viatrix	Vibidia	Victoria	Victorina	Vilbia
Vinicia	Virgilia	Virginia	Viventia	Volumnia	Xiphilina
Zonara""";
    """Abercius	Abito	Acacius	Acaunus	Acilianus	Adauctus
Adepphius	Adjutor	Adranos	Adventus	Aeacus	Aebutus
Aelianus	Aemilianus	Afer	Agapitus	Agatopus	Agelastus
Agorix	Agricola	Agrippa	Agustalis	Ahala	Albinius
Albinus	Albucius	Alethius	Aloysius	Aluredes	Alypius
Amandus	Amantius	Ambrosius	Ammausius	Ammianus	Amor
Amphion	Ampliatus	Ancus	Angelus	Annaeus	Antistianus
Antyllus	Anullinus	Apelles	Apellinus	Aper	Apollonarius
Aponius	Apuleius	Aquila	Aquilius	Aquillius	Aratus
Arcavius	Archarius	Arius	Armiger	Arpagius	Arrianus
Arruntius	Aruns	Arvina	Asclepiodotus	Asellio	Asiagenes
Asina	Asprenas	Asprenus	Assanius	Athanasius	Audaios
Audens	Augendus	Augurinus	Augurius	Augustales	Augustalis
Augustanus	Augustus	Auila	Aurelianus	Aurelius	Ausonius
Auspex	Auxentius	Auxientius	Auxilius	Avitus	Balbillus
Balbus	Balduinus	Bambalio	Bamballio	Banquerius	Barbatus
Baro	Bartolomaeus	Bassus	Bato	Belenus	Belisarius
Bellator	Belletor	Bellicus	Bellus	Benigius	Bestia
Betto	Bibaculus	Bibulus	Bitucus	Blaesus	Blandus
Blassianus	Bodenius	Bolanus	Bonosus	Bonus	Bradua
Bromidus	Bruccius	Brucetus	Bruscius	Brutus	Bubo
Bulla	Burcanius	Burrus	Buteo	Caecilianus	Caecina
Caecus	Caelestis	Caelestius	Caelianus	Caelinus	Caepio
Caerellius	Caesar	Caesoninus	Calacicus	Calatinus	Caldus
Calenus	Calerus	Caletus	Caligula	Callisunus	Calogerus
Calpornius	Calpurnianus	Calpurnis	Calvinus	Calvus	Camerius
Camillus	Campanus	Candidus	Canidius	Canio	Canisius
Cantaber	Capito	Capiton	Caprarius	Caracturus	Carantus
Carausius	Carbo	Carisius	Carius	Carnifex	Casca
Cassianus	Castorius	Castus	Catianus	Catilina	Cato
Catonius	Catsuminianus	Catullus	Catulus	Catus	Cecilianus
Celer	Celsus	Cenaeus	Cencius	Censorinus	Censorius
Centumalus	Cerialis	Cervianus	Cervidus	Cethegus	Chilo
Christianus	Cicero	Cico	Cimber	Cinna	Cinnianus
Cita	Cittinus	Civilis	Clarus	Classicianus	Classicus
Claudianus	Clemens	Clement	Clodian	Clodianus	Cocceianus
Cogitatus	Colias	Collatinus	Columbanus	Columella	Comes
Comitianus	Comitinus	Commidius	Commidus	Commius	Compitalicius
Concessus	Condrausisius	Congrio	Consrotius	Constans	Constantius
Contumeliorus	Corbulo	Cordus	Cornix	Cornutus	Corvinus
Cotentinus	Cotta	Crassus	Cremutius	Crescentius	Cresces
Crispian	Crispin	Crispus	Crito	Crotilo	Cucuphas
Culleolus	Cumanus	Cunctator delayer	Cunobarrus	Cupitus	Curio
Cyprianus	Cyprias	Dacian	Dagwalus	Dama	Damasippus
Dannicus	Dardanius	Dardanus	Decianus	Decmitius	Decmus
Decuminus	Delicius	Desideratus	Dexion	Dexippus	Didacus
Dignus	Dio	Diocletianus	Dioscourides	Disertus	Docilinus
Docilis	Dolabella	Dominicus	Domitianus	Donatianus	Donatus
Donicus	Drusillus	Drusus	Dubitatus	Durio	Durus
Duvianus	Eborius	Eburnus	Ecdicius	Eclectus	Egbutius
Egnatius	Elerius	Eleutherius	Eliphias	Elvorix	Emeritus
Encratis	Ennecus	Ennius	Eonus	Epidianus	Epimachus
Epitynchianus	Epolonius	Erasinus	Esdras	Eudomius	Eugenius
Eugenus	Eumenius	Eunapius	Eustacius	Eutherius	Evodius
Excingus	Exsupereus	Exuperantius	Exupertus	Fabianus	Fabillus
Facilis	Fadus	Fagus	Falco	Falconius	Falx
Famia	Familiaris	Fastidius	Faustillus	Faustinianus	Faustinius
Faustus	Felicissimus	Felissimus	Felix	Ferentinus	Ferreolius
Festus	Fidelis	Figulus	Fimbria	Fimus	Firminus
Firmus	Flaccus	Flavian	Flavianus	Flavillus	Flavinus
Florens	Florianus	Forianus	Fortunatus	Fraucus	Fredisius
Frigidian	Frontalis	Frontinus	Fronto	Fructosis	Frugi
Frugius	Frumentius	Fullofaudes	Fulvianus	Funisulanus	Furius
Fuscinus	Fuscus	Gaianus	Gaius	Gala	Galarius
Galerus	Gallus	Galvisius	Garilianus	Gaurus	Gavros
Gavrus	Gelasius	Gellius	Gemellus	Geminianus	Generidus
Genesius	Genialis	Gerardus	Gessius	Geta	Getha
Glabrio	Glaucia	Globulus	Gluvias	Glycia	Gordianus
Gordio	Gorgonius	Gracchus	Gracilis	Gratidianus	Grumio
Gualtierus	Habitus	Hadrianus	Hardalio	Haterius	Helvius
Herculius	Herenus	Herma	Hermina	Hesychius	Hiberus
Hieronimianus	Hilario	Hilaris	Hilarius	Hirpinius	Hirrus
Homullus	Horatius	Hortensius	Hosidius	Humilis	Hyacinthus
Hybrida	Iacomus	Igennus	Indaletius	Indus	Ingenuus
Ingenvinus	Iocundus	Isatis	Italicus	Ivimarus	Januarius
Javolenus	Jovinianus	Jovinus	Jovius	Juba	Julian
Julianus	Juncinus	Juncus	Junianus	Justianus	Justin
Justinianus	Justinus	Justus	Juvenalis	Kaeso	Lactantius
Laeca	Laenas	Laetinianus	Laevinus	Larcius	Lartius
Lateranus	Latinus	Laurentius	Leddicus	Lentullus	Lentulus
Leon	Lepidus	Lepontus	Leptis	Libanius	Liberalis
Libo	Licinianus	Licinius	Ligur	Ligustinus	Limetanus
Linus	Litorius	Littera	Litumarus	Livianus	Livigenus
Lovernianus	Lovernius	Lucan	Lucanus	Lucianus	Lucilianus
Lucretinaus	Lucretius	Luctacus	Lucullus	Lunaris	Luonercus
Lupercus	Lupicinus	Lupinus	Lupus	Lurco	Lurio
Lutherius	Lutorius	Maccalus	Macrinus	Macro	Macrobius
Mactator	Maecenas	Maecius	Magnus	Magunnus	Maius
Major	Malchus	Mallus	Maltinus	Maminianus	Mancinus
Manlius	Mansuetus	Marcellinus	Marcellus	Marcialis	Marcipor
Margarita	Marinianus	Marinus	Maritimus	Marius	Maro
Marsicus	Marsus	Marsyas	Martial	Martialis	Martianus
Martinus	Martius	Marullinus	Marullus	Maternus	Matho
Maursus	Maximian	Maximinius	Maximus	Medullinus	Megellus
Melissus	Melitus	Mellitus	Melus	Meminius	Memmius
Memor	Mercator	Mercurialis	Mercurinus	Merula	Messala
Messor	Metellus	Metilius	Metunus	Micianus	Mico
Milonius	Minervalis	Minianus	Minicianus	Moderatus	Molacus
Momus	Montanus	Montaus	Mordanticus	Mucianus	Muco
Muncius	Murena	Mus	Musa	Musicus	Mutilus
Mutius	Nabor	Naevius	Namatianus	Narcissus	Nasica
Naso	Natalinus	Natalis	Naucratius	Nazarius	Nectaridus
Nelius	Nemesianus	Nemnogenus	Neneus	Nennius	Nepos
Nero	Nertomarus	Nerva	Nicasius	Nigellus	Niger
Nigidius	Nigrinus	Niraemius	Nolus	Nonius	Noster
Novatian	Novellius	Numerianus	Numonis	Oceanus	Octavian
Octavianus	Octobrianus	Olennius	Olympicus	Opimius	Opis
Optatus	Orientalis	Orientius	Orissus	Orosius	Ostorianus
Pacatianus	Pachomius	Pacuvianus	Paenula	Paetinus	Paetus
Palicamus	Pamphilius	Panaetius	Pansa	Pantenus	Pantera
Panthera	Papinian	Papus	Paratus	Parnesius	Pascentius
Paterculus	Patiens	Paulinus	Paullus	Pavo	Pennus
Peregrinus	Perennis	Perpenna	Perperna	Pertacus	Pertinax
Petasius	Petreius	Petronax	Petrus	Philippus	Pictor
Pilatus	Pilus	Pinarius	Piso	Pius	Placidus
Planta	Plautis	Plautius	Plautus	Pleminius	Pollienus
Pollio	Polus	Polybius	Pompolussa	Pomponius	Poplicola
Porcus	Porphyrius	Postumianus	Postumus	Potitus	Praetextus
Prilidianus	Primanus	Primulus	Primus	Prisca	Priscillian
Priscillianus	Priscus	Processus	Proceus	Proculus	Procyon
Profuterius	Propertius	Propinquus	Protacius	Protus	Proxsimus
Publianus	Publicola	Pudens	Pudentius	Pulcher	Pulcherius
Pullus	Pusinnus	Pustula	Quartinus	Quarto	Quatruus
Quentin	Quietus	Quintilianus	Quintilius	Quintillius	Quintillus
Quiriac	Quiricus	Quirinalis	Ramio	Ramirus	Ravilla
Reburrus	Receptus	Rectus	Regillus	Reginus	Regulus
Remigius	Remus	Renatus	Respectus	Restitutus	Rex
Ripanus	Rogelius	Romanus	Romulianus	Romulus	Roscius
Rufinianus	Rufinus	Rufrius	Rufus	Rullus	Ruricius
Ruso	Rusticus	Rutilianus	Sabellius	Sabinianus	Sabinus
Saenus	Salinator	Salonianus	Saloninus	Salonius	Salvianus
Salvius	Sanctus	Sandilianus	Sanga	Sarimarcus	Sarrius
Saturninus	Saunio	Scaevola	Scapula	Scaro	Scato
Scaurus	Schlerus	Scipio	Scribonianus	Scrofa	Secundus
Segestes	Sejanus	Sellic	Seneca	Senecianus	Senecio
Senilis	Senna	Senopianus	Sentius	Septimianus	Sergius
Seronatus	Serranus	Sertorius	Servanus	Servatius	Servilius
Seuso	Severinus	Sevso	Siculus	Sidonius	Sigilis
Silanus	Silius	Silo	Silus	Silvanus	Similis
Simo	Simplex	Simplicianus	Siricus	Sisenna	Sisinnius
Sita	Sollemnis	Sorex	Sorio	Sosius	Soterichus
Sotericus	Soulinus	Spartacus	Spendius	Speratus	Statius
Stichus	Strabo	Sudrenus	Suilius	Sulinus	Sulla
Sulpicius	Super	Superbus	Superstes	Sura	Surinus
Surius	Surus	Sylla	Sylvian	Sylvius	Symmachus
Symphorian	Sympronian	Synistor	Synnodus	Tacitus	Taenarus
Tancinus	Tanicus	Tarquinius	Tarsicius	Tasius	Tatian
Tatianus	Taurinus	Taurus	Telesinus	Terenteianus	Tertius
Tertullian	Tertullianus	Tertulus	Tetricus	Tetullianus	Thrasea
Tiberillus	Tiberinus	Tibullus	Tiburs	Tiburtius	Ticinus
Titianus	Titillus	Torquatus	Toutius	Traianus	Traillus
Tranio	Tranquillus	Trebellius	Trebius	Trebonianus	Trebonius
Tremerus	Tremorinus	Trenico	Trenus	Triarius	Trifer
Triferus	Trimalchio	Trogus	Trupo	Tuccianus	Tuditanus
Tullius	Tullus	Turibius	Turpilianus	Turpilinus	Turpilius
Tuticanus	Tutor	Typhoeus	Tyranus	Ulfila	Ulixes
Ulpian	Umbonius	Ursacius	Ursinus	Ursus	Uticensis
Vala	Valens	Valentinian	Valentinus	Valerianus	Valgus
Varialus	Varro	Varus	Vatia	Vedrix	Vegetius
Velus	Venator	Ventor	Venustinius	Vepgenus	Veranius
Verecundus	Vergilius	Verinus	Verres	Verrucosis	Verullus
Verulus	Verus	Vespasianus	Vestinus	Vestorius	Vetranio
Vettonianus	Veturius	Vibennis	Vibius	Vibullius	Victor
Victricius	Vincentius	Vindex	Vinicianus	Vipsanius	Virginius
Viridio	Virilis	Virnius	Vitalinus	Vitalion	Vitalis
Vitoricus	Vitulus	Vitus	Vocula	Volturcius	Volusenus
Volusianus	Vonones	Vopiscus	Voteporix	Vulso	Zosimus
Africanus	Allobrogicus	Asiaticus	Atticus	Balearicus	Briganticus
Britannicus	Creticu	Cyriacus?	Dalmaticus	Gaetulicus?	Gallicus
Germanicus	Glycoricus?	Helveticus	Isauricus	Macedonicus	Numidicus
Spartacus
Primus, Primitivus
Secundus
Tertius
Quartius, Quartilius
Quintus, Quintilius
Sextus
Septimus
Octavius
Nonus
Decimus
Undecimus
Vicesimus""";
"""Balearica	Cretica	Dalmatica
Prima, Primitiva, Una
Secunda
Tertia
Quartia, Quartilla
Quinta, Quintilla
Sexta
Septima
Octavia
Nona
Decima
Undecima
Vicesima
Aeliana	Aeterna	Afrania	Afrodisia	Agneta	Agrippina
Aia	Amabilis	Amalthea	Amanda	Apicata	Apphia
Apronia	Apulia	Arruntia	Astia	Atia	Augusta
Aventina	Avita	Blatta	Byrria	Caepionis	Caesonia
Calva	Camilla	Capitolina	Carina	Casta	Cata
Catia	Caula	Censorina	Cherusca	Christiana	Cinna
Clara	Cloelia	Columella	Concessa	Cordelia	Cosconia
Cotta	Coventina	Crescentia	Crispina	Cristina	Cypria
Decima	Desiderata	Dinysia	Dolichena	Domna	Dulcilla
Emelia	Emerentiana	Encratia	Ennia	Erigena	Estella
Eubala	Eubia	Eutropia	Fausta	Faustina	Faustinia
Fecenia	Felicia	Felicula	Fesonia	Flora	Fontia
Fortunata	Gaea	Gaetulica	Galla	Geneura	Gloria
Gnaea	Graecina	Grata	Hilara	Hilaria	Honora
Hypatia	Iana	Idonea	Indara	Ingenua	Innocentia
Januaria	Jovina	Juliana	Julilla	Julitta	Juncina
Justa	Justina	Lanilla	Larina	Laureola	Laurina
Lavinia	Lepida	Lesbia	Licinian	Livigena	Longina
Luciana	Lucilla	Lucina	Lupula	Lutatia	Macrina
Maesa	Maia	Mallea	Mamaea	Mamma	Mammiola
Mansueta	Mariana	Marina	Martia	Martina	Martiola
Matrona	Maxilla	Maxima	Maximilla	Messalina	Messina
Metella	Metiliana	Milvia	Myrtilla	Naissa	Narcissa
Natalia	Nobilioris	Numantina	Occia	Ocresia	Olivia
Oriuna	Pacata	Palla	Panthea	Pantheria	Pascentia
Paulla	Peregrina	Petraea	Phrygia	Pinta	Placida
Planasia	Plancina	Plautia	Plotina	Pluma	Pompeia
Poppaea	Porcella	Postimia	Potamiaena	Praxedes	Prenestina
Priscilla	Procilla	Procula	Pulchra	Pussitta	Regina
Restita	Rhea	Ria	Romana	Sabina	Sabrina
Sacrata	Salonia	Salviena	Sapientia	Saturnia	Saturnina
Scaura	Scipionis	Secunda	Selene	Selina	Serena
Severa	Severiana	Silana	Silicia	Silvia	Simplicia
Sophrona	Sosia	Sulpicia	Tacita	Tarpeia	Tebetta
Teracina	Tertia	Tertulla	Thena	Tiberia	Titullinia
Trifosa	Tristonia	Tuomina	Ulidia	Una	Ursula
Utica	Vacia	Vecchia	Velva	Velvinna	Vernico
Veronica	Viatrix	Vibidia	Victoria	Victorina	Vilbia
Vinicia	Virgilia	Virginia	Viventia	Volumnia	Xiphilina
Zonara""";
    """Accius?	Acilius	Aebutius	Aedinius	Aegidius	Aelia/Aelius
Aeresius	Aetius	Afrania/Afranius	Agrius	Albanus	Albia/Albius
Albinovanus	Albucius	Alfenus	Allectus	Alleius	Allia/Allius
Amatius	Ammianus	Ancharius	Annaeus	Annia/Annius	Anninius
Antistius	Antius	Antonia/Antonius	Antoninus	Appuleius	Arius
Arminia/Arminius	Arminus	Arria/Arrius	Arruntia/Arruntius	Artorius	Asinius
Ateius	Atia/Atius	Atilius	Atrius	Attia/Attius	Aufidius
Aulus	Aurunceius	Ausonius	Autronius	Avidius	Axius
Babudius	Baebius	Baenius	Baibius	Barrius	Bebius
Belaeus	Bellienus	Blandius	Bruccius	Bruttius	Caelius
Caeparius	Caerellius	Caesennius	Caesius	Calatoria/Calatorius	Caledonius
Calidius	Calpurnia/Calpurnius	Calventius	Calvinus	Calvisius	Cammius
Canuleius	Caprenius	Caria/Carius	Caristanius	Caristianus	Cassia/Cassius
Cassianus	Catiotus	Cecia/Cecius	Celatus	Celerinius	Centennius
Cicereius	Cipius	Clitumna/Clitumnus	Cloatius	Clovius	Cluentius
Cluntius	Cnisius	Cocceius	Cominius	Comnena/Comnenus	Conconius
Congaonius	Congonius	Cordius	Cornificius	Cosconius	Cossutia/Cossutius
Crispus	Curatia/Curatius	Curius	Curtius	Decianus	Decimius
Decumius	Delluius	Desidenius	Desticius	Dexius	Didius
Dillius	Dossenius	Drusa/Drusus	Duccius	Duilis	Duilius
Duronius	Egnatius	Ennius	Epidius	Equitia/Equitius	Fabricius
Fadia	Fadius	Falerius	Famulus	Fannius	Fausta/Faustus
Faventinus	Favonius	Fenius	Festinius	Flaccus	Flaminius
Flavinius	Flavonius	Floridius	Florius	Floronius	Fonteia/Fonteius
Francus	Fufius	Fulcinia/Fulcinius	Fulvia/Fulvius	Fundanus	Fundilius
Funisulanus	Gabinius	Galenus	Galerius	Galla/Gallus	Gargilius
Gavius	Gellius	Grania/Granius	Gratia/Gratus	Gratidia/Gratidius	Haterius
Helvetius	Helvia/Helvius	Herennius	Herius	Herminius	Hispala/Hispalus
Horatia/Horatius	Hortensia/Hortensius	Hosidius	Hostilius	Inventius	Iulus
Javolena/Javolenus	Jucundius	Junia/Junius	Justus	Juventius	Laberius
Labienus	Laelius	Laetonius	Lafrenius	Lampronius	Lepidus
Liburnius	Licinia/Licinius	Ligustinius	Lollia/Lollius	Longinus	Longus
Loreius	Lucceia/Lucceius	Lucia/Lucius	Lucilia/Lucilius	Lusius	Lutatius
Maccius	Macrinus	Maecilius	Maelius	Maenia/Maenius	Magius
Maianius	Mallius	Mamilius	Manilius	Manlius	Mannius
Marcia/Marcius	Maria/Marius	Martiannius	Matia/Matius	Maximius	Melissaeius
Memmius	Messienus	Metilius	Milonius	Minucius	Minutius
Modius	Mucia/Mucius	Mummius	Munatius	Munius	Murcius
Murrius	Naevius	Nasennius	Nemetorius	Nepius	Neratius
Nigidius	Nigidullus	Nigilius	Nigrius	Nipius	Nonius
Norbanus	Novius	Numerius	Occius	Oclatinius	Octavia/Octavius
Olcinius	Oppius	Opsius	Oranius	Orestilla	Ostorius
Otacilius	Papellius	Papius	Paquius	Peltrasius	Perpennia/Perpennius
Perquitienus	Pescennius	Petellius	Petilius	Petillius	Petreia/Petreius
Petronia/Petronius	Piscius	Pisentius	Pituanius	Placida/Placidus	Platorius
Plautia/Plautius	Plinius	Plotius	Poenius	Pollia/Pollius	Polus
Pomponia/Pomponius	Pomptinus	Pontidius	Pontius	Popidius	Popillia/Popillius
Poppaedius	Portia/Portius	Praesentius	Publicius	Pupius	Quettius
Quinctilia/Quinctilius	Quintilia/Quintilius	Quintius	Quirinius	Rabirius	Roscius
Rufia/Rufius	Rufina/Rufinus	Rufrius	Rufus	Rusonia/Rusonius	Sabidius
Sabucius	Sacerdus	Sallustius	Salonia/Salonius	Salvius	Saufeius
Scribonia/Scribonius	Secundinius	Secundius	Seius	Senecianus	Senicianus
Sennius	Sentius	Septimius	Sepunius	Sepurcius	Sertoria/Sertorius
Sestius	Sextilius	Sextius	Sidonius	Silia/Silius	Silvianus
Sittius	Socellius	Sornatius	Sosia/Sosius	Spurius	Staius
Statius	Statlilius	Stertinius	Stlaccius	Suedius	Sylvia/Sylvius
Tadia/Tadius	Talmudius	Tanicius	Teideius	Terentia	Terentius
Tertinius	Tetius	Titia/Titius	Titinia/Titinius	Tituleius	Tragus
Trebatius	Trebellius	Tremellius	Tuccius	Tullia/Tullius	Turrianus
Ulpianus	Ulpius	Umbrenius	Urgulania/Urgulanius	Uulius	Vagiennius
Vagionius	Vagnius	Valerianus	Valerius	Valgus	Vargunteius
Varia/Varius	Vassinus	Vatinius	Vedius	Velva/Velvus	Venidius
Veranius	Verecundius	Vergilius	Verus	Vesnius	Vesuius
Vesuvius	Vettienus?	Vibenius	Vibidius	Victricius	Vidacilius
Viducius	Vinicius	Vipsania/Vipsanius	Viridius	Virius	Vitellia/Vitellius
Vitruvius	Vitulasius	Volcatius	Volumni/Volumnius	Volusenus	Volusia/Volusius
Aemilia/Aemilius	Atilia/Atilius	Aurelia/Aurelius	Caecilia/Caecilius	Camilla/Camillus	Claudia/Claudius
Clodia/Clodius	Cornelia/Cornelius	Domitia/Domitius	Fabia/Fabius	Flavia/Flavius	Furia/Furius
Livia/Livius	Manlia/Manlius	Papinia/Papinius	Papiria/Papirius	Pinarus	Pompeia/Pompeius?
Porcia/Porcius?	Postumia/Postumius	Rutilia/Rutilius	Sempronia/Sempronius	Segia/Sergius	Servilia/Servilius
Sulpicia/Sulpicius	Valeria/Valerius	Vettia/Vettius?
""";
    ]

let orcMale = "Gronk Sponk Spud Thud Blat Kreegah Krock Skar Smash Szeth Vallano"

let trim (s:string) = s.Trim()
let preprocess name =
    System.Text.RegularExpressions.Regex.Replace(trim name, "[.,]*","")
let parseLine (separator: char) (ln:string) =
    (preprocess ln).Split(separator)
let isValid (txt:string) =
    System.Text.RegularExpressions.Regex.IsMatch(txt, "^[A-Z][a-zA-Z]+$") // filter out weird celtic characters, names that don't start with a capital, and other things that look like errors
// parse1=discard all but first column
let parse1 (input:string) =
    let parseLine = parseLine ' '
    input.Split('\n') |> Seq.map(fun (ln:string) ->
        match ln.IndexOf('\t') with
        | -1 -> parseLine ln
        | x -> ln.Substring(0, x) |> parseLine
        )
    |> Seq.collect id
    |> Seq.filter isValid
    |> List.ofSeq
let parseAll (input:string) =
    input.Split('\n')
    //|> Seq.map (fun x -> x.Replace("\t", " "))
    |> Seq.map (parseLine '\t')
    |> Seq.collect id
    |> Seq.filter isValid
    |> List.ofSeq
let differentiate ix (name:string) =
    match name.Split('/') with
    | [|_|] -> name // no split
    | names when names.Length > ix -> names.[ix]
    | _ -> failwithf "I don't understand this: %s" name
let parseTabbedWithDifferentiation index (input:string) =
    input.Split('\n')
    |> Seq.map (parseLine '\t')
    |> Seq.collect id
    |> Seq.map (differentiate index)
    |> Seq.filter isValid
    |> List.ofSeq

let join sep lines = System.String.Join((sep:string), (lines:string seq))
let codeFor tables =
    tables |> Seq.map
        (fun (tableName, tableType, parser, (input:string)) ->
            let render lst =
                [
                    let src = lst |> Array.ofSeq
                    let chunkSize = 100
                    let chunkNumber = src.Length + (chunkSize - 1) / chunkSize
                    for chunkStart in 0..chunkSize..src.Length-1 do
                        let items = src.[chunkStart..(min src.Length (chunkStart + chunkSize))-1]
                        yield join ";" (items |> Seq.map (sprintf "%A"))
                    ]
                |> join ";\n        "
            sprintf "    (%A, %A), [|\n        %s\n        |]" tableName tableType (parser input |> render))
    |> join "\n"
    |> System.Windows.Forms.Clipboard.SetText

codeFor [
    "Tir na n'Og", "Male", parse1, cmale; "Tir na n'Og", "Female", parse1, cfemale; "Tir na n'Og", "Last", parse1, clast
    "Abysia", "Male", parseAll, memale; "Abysia", "Female", parseAll, mefemale
    "Kailasa", "Male", parseAll, hmale; "Kailasa", "Female", parseAll, hfemale; "Kailasa", "Last", parseAll, hlast
    "Ermor", "Male", parseAll, rome.[0]; "Ermor", "Female", parseAll, rome.[1]; "Ermor", "CognomenMale", parseAll, rome.[2]; "Ermor", "CognomenFemale", parseAll, rome.[3]; "Ermor", "LastFemale", parseTabbedWithDifferentiation 0, rome.[4]; "Ermor", "LastMale", parseTabbedWithDifferentiation 1, rome.[4];
    "Undauntra", "Male", parseAll, greece.[0]; "Undauntra", "Female", parseAll, greece.[1];
    "Arboria", "Male", parseAll, english.[0]; "Arboria", "Femalale", parseAll, english.[1]; "Arboria", "Last", parseAll, english.[2];
    "Mordor", "Male", parseAll, orcMale.Replace(" ", "\t")
            ]

