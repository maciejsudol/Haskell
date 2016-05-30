import qualified Data.Map as Map	-- importujemy map i nadajemy jej kwalifikator
import qualified Data.Maybe as Maybe	
import Data.Map (Map)			-- tylko map

-------------------------------------------------------------------

data Gracz = Gracz Marker deriving (Show, Eq)
data Gra = Gra StanGry [Gracz] deriving (Show, Eq)
data Plansza = Plansza Rozmiar PozycjePionkow deriving (Eq)
data StanGry = StanGry Plansza Gracz deriving (Show, Eq)
data Marker = Brak | Czarny | Bialy | Damka Marker deriving (Show, Eq)
data Ruch = Ruch Pozycja Pozycja deriving (Show, Eq, Read)

type Rozmiar = (Int, Int)
type Pozycja = (Int, Int)
type PozycjePionkow = Map Pozycja Marker

-------------------------------------------------------------------

instance Show Plansza where
	show plansza@(Plansza (szerokosc, _) pozycje) =
		polacz wiersze where
			polacz = foldr (\p1 p2 -> p2 ++ ('\n':p1)) ""
			pozycjeNaPlanszy = pozycjePlanszy plansza
			markery = concat $ map (\p -> markerString $ pozycje Map.! p) pozycjeNaPlanszy
			wiersze = numerKolumny 8 $ numerWiersza 1 $ podzielNaKawalki szerokosc markery
			numerKolumny licznik wartosci = wartosci ++ ["  " ++ (foldr (\p1 p2 -> p2 ++ show p1) "" (reverse [1..licznik]))]
			numerWiersza poczatek (glowa:ogon) = (show poczatek++ " " ++ glowa) : numerWiersza (poczatek+1) ogon
			numerWiersza _ [] = []
			podzielNaKawalki _ [] = []
			podzielNaKawalki dzielnik wartosci = czesc1 : podzielNaKawalki dzielnik czesc2 where
				(czesc1, czesc2) = splitAt dzielnik wartosci

markerString :: Marker -> String
markerString marker = case marker of
	Brak	-> "-"
	Czarny	-> "c"
	Bialy	-> "b"
	Damka Czarny	-> "C"
	Damka Bialy	-> "B"

-------------------------------------------------------------------

pozycjePlanszy :: Plansza -> [Pozycja]
pozycjePlanszy (Plansza (szerokosc, wysokosc) _) =
	[(kolumna, wiersz) | kolumna <- [1..szerokosc], wiersz <- [1..wysokosc]]

zwrocPlansze :: Rozmiar -> Plansza
zwrocPlansze rozmiar =
	Plansza rozmiar (zwrocPozycjePionkow rozmiar)

zwrocPozycjePionkow :: Rozmiar -> PozycjePionkow
zwrocPozycjePionkow (szerokosc, wysokosc) =
	Map.fromList $ zip pozycje (repeat Brak) where
		pozycje = [(wiersz, kolumna) | wiersz <- [1..wysokosc], kolumna <- [1..szerokosc]]

markerNaPozycji :: PozycjePionkow -> Pozycja -> Marker
markerNaPozycji pozycje pozycja =
	maybe Brak id (Map.lookup pozycja pozycje)

zamienPozycje :: PozycjePionkow -> Pozycja -> Pozycja -> PozycjePionkow
zamienPozycje pozycje staraPozycja nowaPozycja =
	Map.insert staraPozycja (markerNaPozycji pozycje nowaPozycja) $ Map.insert nowaPozycja (markerNaPozycji pozycje staraPozycja) pozycje

rozmiar :: Plansza -> Rozmiar
rozmiar (Plansza rozmiar _) =
	rozmiar

podstawowyMarker :: Marker -> Marker
podstawowyMarker (Damka marker) =
	podstawowyMarker marker
podstawowyMarker Czarny = Czarny
podstawowyMarker Bialy = Bialy
podstawowyMarker Brak = Brak

zwrocMarkery :: Plansza -> PozycjePionkow
zwrocMarkery (Plansza _ markery) =
	markery

-- aktualizuje markery
zamienMarkery :: Plansza -> PozycjePionkow -> Plansza
zamienMarkery (Plansza rozmiar _) noweMarkery =
	(Plansza rozmiar noweMarkery)

markerIPozycja :: Marker -> Int -> Int -> (Pozycja, Marker)
markerIPozycja marker wiersz kolumna =
	((wiersz, kolumna), marker)

-- aktualizuje pozycje markera na planszy
aktualizujPlansze :: Plansza -> Pozycja -> Marker -> Plansza
aktualizujPlansze plansza pozycja marker =
	nowaPlansza where
		stareMarkery = zwrocMarkery plansza
		noweMarkery = Map.insert pozycja marker stareMarkery
		nowaPlansza = zamienMarkery plansza noweMarkery

aktualizujPozycje :: Plansza -> [Pozycja] -> Marker -> Plansza
aktualizujPozycje plansza pozycje marker = nowaPlansza where
	nowaPlansza = foldr funkcja plansza pozycje
	funkcja pozycjA planszA = aktualizujPlansze planszA pozycjA marker
	
pustePozycje :: Plansza -> [Pozycja]
pustePozycje (Plansza _ markery) =
	Map.keys $ Map.filter (== Brak) markery

wygral :: Plansza -> Marker -> Bool
wygral _ Brak = False
wygral plansza (Damka marker) = wygral plansza marker
wygral (Plansza _ markery) marker =
	not istniejeInnyMarker where
		istniejeInnyMarker = any (funkcja innyMarker) elementy
		innyMarker = podstawowyMarker marker
		funkcja marker1 marker2 = podstawowyMarker marker1 == marker2
		elementy = Map.elems markery

koniecGry :: Gra -> Bool
koniecGry gra =
	any (wygral plansza) markery where
		plansza = planszaDoGry gra
		markery = [Bialy, Czarny]

planszaDoGry :: Gra -> Plansza
planszaDoGry (Gra (StanGry plansza _) _) =
	plansza

ktoWygral :: Gra -> Marker
ktoWygral gra@(Gra (StanGry plansza (Gracz marker)) _) =
	winner where
		winner = if koniecGry gra then
			if marker == Bialy
				then Czarny
				else Bialy
		else Brak

-- wymazuje wskazana pozycje
wymazPozycje :: StanGry -> Pozycja -> StanGry
wymazPozycje gra pozycja =
	zmienPozycjeStanu gra pozycja Brak

-- zamienia dana pozycje z podanym argumentem
zmienPozycjeStanu :: StanGry -> Pozycja -> Marker -> StanGry
zmienPozycjeStanu (StanGry staraPlansza gracz) pozycja marker =
	(StanGry nowaPlansza gracz) where
		nowaPlansza = aktualizujPlansze staraPlansza pozycja marker

zwrocMarkerStanu :: StanGry -> Pozycja -> Marker
zwrocMarkerStanu (StanGry plansza _) pozycja =
	markerNaPozycji (zwrocMarkery plansza) pozycja

-- aktualizuje stan gry wraz z wykonaniem ruchu
aktualizujStan :: StanGry -> Pozycja -> Pozycja -> StanGry
aktualizujStan (StanGry staraPlansza poprzedniGracz) staraPozycja nowaPozycja =
	(StanGry nowaPlansza nastepnyGracz) where
		poprzedniMarker = markerNaPozycji (zwrocMarkery staraPlansza) staraPozycja
		usunietaPozycja = aktualizujPlansze staraPlansza staraPozycja Brak
		nowaPlansza = aktualizujPlansze usunietaPozycja nowaPozycja poprzedniMarker
		nastepnyGracz = case poprzedniGracz of
			Gracz Czarny		-> Gracz Bialy
			Gracz (Damka Czarny)	-> Gracz Bialy
			_			-> Gracz Czarny

-- sprawdza, czy zadana pozycja jest pusta
pustaPozycja :: Plansza -> Pozycja -> Bool
pustaPozycja plansza pozycja =
	testowanaPozycja == Brak where
		testowanaPozycja = markerNaPozycji (zwrocMarkery plansza) pozycja

-- sprawdza, czy nowa zadana pozycja jest prawidlowa
wGranicach :: Plansza -> Pozycja -> Bool
wGranicach plansza pozycja =
	((x > 0) && (x <= szerokoscPlanszy)) && ((y > 0) && (y <= wysokoscPlanszy)) where
	(szerokoscPlanszy, wysokoscPlanszy) = rozmiar plansza
	(x, y) = pozycja

prawidlowaPozycja :: Plansza -> Pozycja -> Bool
prawidlowaPozycja plansza pozycja =
	(pustaPozycja plansza pozycja) && (wGranicach plansza pozycja)

-- zwraca mozliwe ruchy o jedna pozycje, bez zbijania innych pionkow
prostyRuch :: Plansza -> Pozycja -> [Pozycja]
prostyRuch plansza pozycja =
	filter (prawidlowaPozycja plansza) (mozliweProsteRuchy marker) where
		mozliweProsteRuchy :: Marker -> [Pozycja]
		mozliweProsteRuchy (Damka _) = mozliweProsteRuchy Czarny ++ mozliweProsteRuchy Bialy
		mozliweProsteRuchy Czarny = [(x-1, y+1), (x+1, y+1)]
		mozliweProsteRuchy Bialy = [(x-1, y-1), (x+1, y-1)]
		mozliweProsteRuchy _ = []
		(x, y) = pozycja
		marker = markerNaPozycji (zwrocMarkery plansza) pozycja

-- sprawdza, czy wykonywany ruch odbedzie sie w prawidlowym kierunku
prawidlowyKierunek :: Marker -> Pozycja -> Pozycja -> Bool
prawidlowyKierunek Czarny (_, y1) (_, y2) =
	y1 < y2
prawidlowyKierunek Bialy (_, y1) (_, y2) =
	y1 > y2

zamienKolor :: Marker -> Marker
zamienKolor Czarny =
	Bialy
zamienKolor Bialy =
	Czarny
zamienKolor (Damka marker) =
	(Damka $ zamienKolor marker)
zamienKolor Brak =
	Brak

-- zwraca pozycje oddalone n przekatnych od podanej pozycji
nPrzekatna :: Int -> Pozycja -> [Pozycja]
nPrzekatna n (wiersz, kolumna) =
	[(wiersz+n, kolumna+n), (wiersz-n, kolumna-n), (wiersz+n, kolumna-n), (wiersz-n, kolumna+n)]

-- zwraca mozliwe skoki na zadanej planszy dla pionka z podanej pozycji
skok :: Plansza -> Pozycja -> [Pozycja]
skok plansza pozycja =
	skoki where
		marker = markerNaPozycji (zwrocMarkery plansza) pozycja
		skoki = filter (wGranicach plansza) (mozliweRuchy marker)
		mozliweRuchy :: Marker -> [Pozycja]
		mozliweRuchy Czarny = mozliweRuchyDlaKoloru Czarny pozycja
		mozliweRuchy Bialy = mozliweRuchyDlaKoloru Bialy pozycja
		mozliweRuchy (Damka damkaMarker) = mozliweSkoki
		mozliweRuchy _ = []

		mozliweRuchyDlaKoloru :: Marker -> Pozycja -> [Pozycja]
		mozliweRuchyDlaKoloru marker pozycja =
			filter wKierunku ruchy where
				wKierunku = prawidlowyKierunek (zamienKolor marker) pozycja
				ruchy = mozliweSkoki

		-- zwraca krotki pozycji poczatkowej i po skoku
		--mozliweSkoki :: [Pozycja]
		mozliweSkoki =
			map (\(_, x, _) -> x) skoki where
				skoki = filter (mozeWykonacSkok marker) mozliweSkokiKrotki
				mozliweSkokiKrotki = zip3 przekatna1 przekatna2 pionkiNaPrzekatnej
				przekatna1 = nPrzekatna 1 pozycja
				przekatna2 = nPrzekatna 2 pozycja
				pionkiNaPrzekatnej = map (markerNaPozycji (zwrocMarkery plansza)) przekatna1
				-- sprawdza, czy skok wykonywany jest nad pionkami przeciwnika
				mozeWykonacSkok :: Marker -> (Pozycja, Pozycja, Marker) -> Bool
				mozwWykonacSkok Brak _ = False
				mozeWykonacSkok _ (_, _, (Damka Brak)) = False
				mozeWykonacSkok marker (_, _, (Damka Czarny)) = marker /= Czarny
				mozeWykonacSkok marker (_, _, (Damka Bialy)) = marker /= Bialy
				mozeWykonacSkok marker (_, _, Czarny) = marker /= Czarny
				mozeWykonacSkok marker (_, _, Bialy) = marker /= Bialy
				mozeWykonacSkok _ _ = False

-- dla podanej pozycji na planszy zwraca poprawne ruchy dla pionka. Pionki poruszaja sie do przodu po przekatnych, zas damki poruszaja sie w kazdym kierunku po przekatnych. Pionki i damki moga przeskakiwac inne figury poruszajac sie o dwie przekatne dalej
ruchyNaPlanszy :: Plansza -> Pozycja -> [Pozycja]
ruchyNaPlanszy plansza pozycja =
	(prostyRuch plansza pozycja) ++ (skok plansza pozycja)

-- zwaraca mozliwe ruchy pionka z danej pozycji w zadanym stanie gry
zwrocMozliweRuchy :: StanGry -> Pozycja -> [Ruch]
zwrocMozliweRuchy (StanGry plansza@(Plansza rozmiar pozycje) gracz) pozycja =
	if (kolorPasuje gracz marker)
		then map (Ruch pozycja) (ruchyNaPlanszy plansza pozycja)
	else [] 
	where
		-- sprawdza, czy gracz ma ten sam kolor
		kolorPasuje :: Gracz -> Marker -> Bool
		kolorPasuje (Gracz markerGracza) markerPozycji = (podstawowyMarker markerGracza) == (podstawowyMarker markerPozycji)
		marker = markerNaPozycji pozycje pozycja

zwrocPozycjeGracza :: Gracz -> Plansza -> [Pozycja]
zwrocPozycjeGracza gracz@(Gracz markerGracza) plansza =
	pozycje where
		marker = podstawowyMarker markerGracza
		kolorPasuje :: Pozycja -> Bool
		kolorPasuje pozycja = marker == podstawowyMarker (markerNaPozycji (zwrocMarkery plansza) pozycja)
		pozycje = filter kolorPasuje $ pozycjePlanszy plansza

zwrocMozliweRuchyStanu :: StanGry -> [Ruch]
zwrocMozliweRuchyStanu stan@(StanGry plansza _) =
	ruchy where
		ruchy = concat $ map (zwrocMozliweRuchy stan) pozycje
		pozycje = pozycjePlanszy plansza

zwrocMozliweRuchyGry :: Gra -> [Ruch]
zwrocMozliweRuchyGry (Gra stan _) =
	zwrocMozliweRuchyStanu stan

-- zwraca prawde, jezeli ruch jest skokiem
sprawdzSkok :: Ruch -> Bool
sprawdzSkok (Ruch (_, yStart) (_, yKoniec)) =
	roznica > 1 where
		roznica = abs $ yStart - yKoniec

zwrocWspolrzedneSkoku :: Ruch -> Pozycja
zwrocWspolrzedneSkoku (Ruch (xStart, yStart) (xKoniec, yKoniec)) =
	nowaPozycja where
		nowaPozycja = (xStart+roznicaKolumn, yStart+roznicaWierszy)
		roznicaKolumn = if xStart < xKoniec
			then 1
			else -1
		roznicaWierszy = if yStart < yKoniec
			then 1
			else -1

-- zamienia noramlny marker na damke
rozszerzonyMarker :: Marker -> Marker
rozszerzonYMarker Czarny =
	Damka Czarny
rozszerzonyMarker Bialy =
	Damka Bialy
rozszerzonyMarker marker = marker

-- zamienia pionekna podanej pozycji na damke
zamienNaDamke :: StanGry -> Pozycja -> StanGry
zamienNaDamke stan pozycja =
	nowyStan where
		nowyStan = zmienPozycjeStanu stan pozycja damka
		damka = rozszerzonyMarker $ zwrocMarkerStanu stan pozycja

-- sprawdza, czy pionek na podanej pozycji powienien byc damka
czyDamka :: Marker -> Pozycja -> Bool
czyDamka (Damka _) _ =
	False
czyDamka Czarny (_, 8) =
	True
czyDamka Bialy (_, 1) =
	True
czyDamka _ _ =
	False

wykonajRuch :: Gra -> Ruch -> Gra
wykonajRuch (Gra aktualnyStan gracze) ruch@(Ruch staraPozycja nowaPozycja) =
	nowaGra where
		nowaGra = Gra stan3 gracze
		stan1 = aktualizujStan aktualnyStan staraPozycja nowaPozycja
		stan2 = if sprawdzSkok ruch
			then wymazPozycje stan1 (zwrocWspolrzedneSkoku ruch)
			else stan1
		stan3 = if czyDamka (zwrocMarkerStanu stan2 nowaPozycja) nowaPozycja
			then zamienNaDamke stan2 nowaPozycja
			else stan2

-------------------------------------------------------------------

-- ustanawia poczatkowy wyglad gry
poczatekGry :: Gra
poczatekGry =
	Gra stanPoczatkowy [Gracz Czarny, Gracz Bialy] where
		stanPoczatkowy = StanGry poczatkowaPlansza $ Gracz Bialy

poczatkowaPlansza :: Plansza
poczatkowaPlansza = startowaPlansza where
	pustaPlansza = zwrocPlansze (8, 8)
	startowaPlansza = aktualizujPozycje bialeFigury czarnePola Czarny
	bialeFigury = aktualizujPozycje pustaPlansza bialePola Bialy
	czarnePola = generujPozycje [1..3]
	bialePola = generujPozycje [6..8]
	generujPozycje wiersze = [(x, y) | x <- [1..8], y <- wiersze, or [and [even x, even y], and [odd x, odd y]]]

graj :: Gra -> IO()
graj gra =
	do
		if not (koniecGry gra)
			then kontynuuj
			else zakoncz
		where
			mozliweRuchy = zwrocMozliweRuchyGry gra
			czyPoprawnyRuch ruch = ruch `elem` mozliweRuchy
			kontynuuj =
				do
					putStrLn $ show gra
					putStrLn $ "Mozliwe ruchy:\n" ++ (show mozliweRuchy) ++ "\nPodaj swoj ruch jako: \"Ruch (poczatkowaKolumna, poczatkowyWiersz) (koncowaKolumna, koncowyWiersz)\" lub \"wyjscie\" w celu zakonczenia"
					wejscie <- getLine
					if wejscie == "wyjscie"
						then putStrLn "Koncze gre..."
					else if czyPoprawnyRuch (przekonwertuj wejscie)
						then
							do
								putStrLn $ "Wykonywanie ruchu " ++ wejscie
								graj (wykonajWczytanyRuch gra wejscie)
						else 
							do
								putStrLn $ wejscie ++ " nie jest poprawnym ruchem"
								kontynuuj
			zakoncz =
				do
					putStrLn $ "Koniec gry. Stan koncowy:\n" ++ show gra
					putStrLn $ "Wygral: " ++ show (ktoWygral gra)	++ "\n"

przekonwertuj :: String -> Ruch
przekonwertuj wejscie =
	read wejscie :: Ruch

wykonajWczytanyRuch :: Gra -> String -> Gra
wykonajWczytanyRuch gra wejscie =
	wykonajRuch gra (przekonwertuj wejscie)

main :: IO()
main =
	do
		putStrLn "Warcaby\n"
		graj poczatekGry
	
