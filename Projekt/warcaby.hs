import qualified Data.Map as Map	-- importujemy map i nadajemy jej kwalifikator
import Data.Map (Map)				-- tylko map

-------------------------------------------------------------------

data Gracz = Gracz Kolor deriving (Show, Eq)
data Gra = Gra StanGry [Gracz] deriving (Show, Eq)
data Plansza = Plansza Rozmiar PozycjePionkow deriving (Eq)
data StanGry = StanGry Plansza Gracz deriving (Show, Eq)
data Kolor = Brak | Czarny | Bialy | Damka Kolor deriving (Show, Eq)
data Ruch = Ruch Pozycja Pozycja deriving (Show, Eq, Read)

type Rozmiar = (Int, Int)
type Pozycja = (Int, Int)
type PozycjePionkow = Map Pozycja Kolor

-------------------------------------------------------------------

instance Show Plansza where
	show plansza@(Plansza (szerokosc, _) pozycje) =
		polacz wiersze where
			polacz = foldr (\p1 p2 -> p2 ++ ('\n':p1)) ""
			pozycjeNaPlanszy = pozycjePlanszy plansza
			kolory = concat $ map (\p -> kolorChar $ pozycje Map.! p) pozycjeNaPlanszy	-- Map.! znajduje wartość na zadanej pozycji
			wiersze = numeracjaKolumny szerokosc $ numeracjaWiersza 1 $ podzielNaKawalki (2*szerokosc) kolory
			numeracjaKolumny licznik wartosci = wartosci ++ ["  " ++ (foldr (\p1 p2 -> p2 ++ show p1 ++ " ") "" (reverse [1..licznik]))]
			numeracjaWiersza _ [] = []
			numeracjaWiersza licznik (glowa:ogon) = (show licznik ++ " " ++ glowa) : numeracjaWiersza (licznik+1) ogon
			podzielNaKawalki _ [] = []
			podzielNaKawalki dzielnik kolory = czesc1 : podzielNaKawalki dzielnik czesc2
				where
					(czesc1, czesc2) = splitAt dzielnik kolory

kolorChar :: Kolor -> String
kolorChar kolor = case kolor of
	Brak			-> "- "
	Czarny			-> "c "
	Bialy			-> "b "
	Damka Czarny	-> "C "
	Damka Bialy		-> "B "

-------------------------------------------------------------------

pozycjePlanszy :: Plansza -> [Pozycja]
pozycjePlanszy (Plansza (szerokosc, wysokosc) _) =
	[(kolumna, wiersz) | kolumna <- [1..szerokosc], wiersz <- [1..wysokosc]]

kolorNaPozycji :: PozycjePionkow -> Pozycja -> Kolor
kolorNaPozycji pozycje pozycja =
	maybe Brak id (Map.lookup pozycja pozycje)

podstawowyKolor :: Kolor -> Kolor
podstawowyKolor kolor = case kolor of
	(Damka kolorDamki)	-> podstawowyKolor kolorDamki
	_					-> kolor

zwrocKolory :: Plansza -> PozycjePionkow
zwrocKolory (Plansza _ kolory) =
	kolory

-- aktualizuje kolory
zamienKolory :: Plansza -> PozycjePionkow -> Plansza
zamienKolory (Plansza rozmiar _) noweKolory =
	(Plansza rozmiar noweKolory)

-- aktualizuje pozycje kolora na planszy
aktualizujPlansze :: Plansza -> Pozycja -> Kolor -> Plansza
aktualizujPlansze plansza pozycja kolor =
	nowaPlansza where
		stareKolory = zwrocKolory plansza
		noweKolory = Map.insert pozycja kolor stareKolory
		nowaPlansza = zamienKolory plansza noweKolory

koniecGry :: Gra -> Bool
koniecGry (Gra (StanGry plansza _) _) =
	any (wygral plansza) [Bialy, Czarny] where

		wygral :: Plansza -> Kolor -> Bool
		wygral _ Brak = False
		wygral plansza (Damka kolor) = wygral plansza kolor
		wygral (Plansza _ kolory) kolor =
			not istniejeInnyKolor where
				istniejeInnyKolor = any (funkcja innyKolor) (Map.elems kolory)
				innyKolor = podstawowyKolor kolor
				funkcja kolor1 kolor2 = podstawowyKolor kolor1 == kolor2

ktoWygral :: Gra -> Kolor
ktoWygral gra@(Gra (StanGry plansza (Gracz kolor)) _) =
	wygrany where
		wygrany = if koniecGry gra then
			if kolor == Bialy
				then Czarny
				else Bialy
		else Brak

-- zamienia dana pozycje z podanym argumentem
zmienPozycjeStanu :: StanGry -> Pozycja -> Kolor -> StanGry
zmienPozycjeStanu (StanGry staraPlansza gracz) pozycja kolor =
	(StanGry nowaPlansza gracz) where
		nowaPlansza = aktualizujPlansze staraPlansza pozycja kolor

zwrocKolorStanu :: StanGry -> Pozycja -> Kolor
zwrocKolorStanu (StanGry plansza _) pozycja =
	kolorNaPozycji (zwrocKolory plansza) pozycja

-- sprawdza, czy zadana pozycja jest pusta
pustaPozycja :: Plansza -> Pozycja -> Bool
pustaPozycja plansza pozycja =
	(kolorNaPozycji (zwrocKolory plansza) pozycja) == Brak

-- sprawdza, czy nowa zadana pozycja jest prawidlowa
wGranicach :: Plansza -> Pozycja -> Bool
wGranicach plansza@(Plansza (szerokoscPlanszy, wysokoscPlanszy) _) pozycja@(x, y) =
	((x > 0) && (x <= szerokoscPlanszy)) && ((y > 0) && (y <= wysokoscPlanszy))

-- zwraca mozliwe ruchy o jedna pozycje, bez zbijania innych pionkow
prostyRuch :: Plansza -> Pozycja -> [Pozycja]
prostyRuch plansza pozycja =
	filter (prawidlowaPozycja plansza) (mozliweProsteRuchy kolor) where
		mozliweProsteRuchy :: Kolor -> [Pozycja]
		mozliweProsteRuchy (Damka _) = mozliweProsteRuchy Czarny ++ mozliweProsteRuchy Bialy
		mozliweProsteRuchy Czarny = [(x-1, y+1), (x+1, y+1)]
		mozliweProsteRuchy Bialy = [(x-1, y-1), (x+1, y-1)]
		mozliweProsteRuchy _ = []
		(x, y) = pozycja
		kolor = kolorNaPozycji (zwrocKolory plansza) pozycja

		prawidlowaPozycja :: Plansza -> Pozycja -> Bool
		prawidlowaPozycja plansza pozycja =
			(pustaPozycja plansza pozycja) && (wGranicach plansza pozycja)

-- sprawdza, czy wykonywany ruch odbedzie sie w prawidlowym kierunku
prawidlowyKierunek :: Kolor -> Pozycja -> Pozycja -> Bool
prawidlowyKierunek Czarny (_, y1) (_, y2) =
	y1 < y2
prawidlowyKierunek Bialy (_, y1) (_, y2) =
	y1 > y2

zamienKolor :: Kolor -> Kolor
zamienKolor kolor = case kolor of
	(Damka kolorDamki)	-> zamienKolor kolorDamki
	Bialy				-> Czarny
	Czarny				-> Bialy
	_					-> kolor

-- zwraca mozliwe skoki na zadanej planszy dla pionka z podanej pozycji
skok :: Plansza -> Pozycja -> [Pozycja]
skok plansza pozycja =
	skoki where
		kolor = kolorNaPozycji (zwrocKolory plansza) pozycja
		skoki = filter (wGranicach plansza) (mozliweSkokiFigura kolor)

		mozliweSkokiFigura :: Kolor -> [Pozycja]
		mozliweSkokiFigura Czarny = mozliweSkokiPionek
		mozliweSkokiFigura Bialy = mozliweSkokiPionek
		mozliweSkokiFigura (Damka damkaKolor) = mozliweSkokiDamka
		mozliweSkokiFigura _ = []

		-- zwraca mozliwe pozycje skoku dla normalnego pionka
		mozliweSkokiPionek :: [Pozycja]
		mozliweSkokiPionek =
			skoki where
				skoki = filter (pustaPozycja plansza) ruchy	-- skoki na puste pozycje
				ruchy = map (\(_, x, _) -> x) $ filter (mozeWykonacSkok kolor) mozliweSkokiKrotki
				mozliweSkokiKrotki = zip3 przekatna1 przekatna2 pionkiNaPrzekatnej
				przekatna1 = nPrzekatna 1 pozycja
				przekatna2 = nPrzekatna 2 pozycja
				pionkiNaPrzekatnej = map (kolorNaPozycji (zwrocKolory plansza)) przekatna1

				-- sprawdza, czy skok wykonywany jest nad pionkami przeciwnika
				mozeWykonacSkok :: Kolor -> (Pozycja, Pozycja, Kolor) -> Bool
				mozwWykonacSkok Brak _ = False
				mozeWykonacSkok _ (_, _, (Damka Brak)) = False
				mozeWykonacSkok kolor (_, _, (Damka Czarny)) = kolor /= Czarny
				mozeWykonacSkok kolor (_, _, (Damka Bialy)) = kolor /= Bialy
				mozeWykonacSkok kolor (_, _, Czarny) = kolor /= Czarny
				mozeWykonacSkok kolor (_, _, Bialy) = kolor /= Bialy
				mozeWykonacSkok _ _ = False

		-- zwraca mozliwe pozycje skoku dla damki
		mozliweSkokiDamka :: [Pozycja]
		mozliweSkokiDamka =
			map (\(_, x, _) -> x) skoki where
				skoki = filter (mozeWykonacSkok kolor) mozliweSkokiKrotki
				mozliweSkokiKrotki = zip3 przekatna1 przekatna2 pionkiNaPrzekatnej
				przekatna1 = nPrzekatna 1 pozycja
				przekatna2 = nPrzekatna 2 pozycja
				pionkiNaPrzekatnej = map (kolorNaPozycji (zwrocKolory plansza)) przekatna1
				-- sprawdza, czy skok wykonywany jest nad pionkami przeciwnika
				mozeWykonacSkok :: Kolor -> (Pozycja, Pozycja, Kolor) -> Bool
				mozwWykonacSkok Brak _ = False
				mozeWykonacSkok _ (_, _, (Damka Brak)) = False
				mozeWykonacSkok kolor (_, _, (Damka Czarny)) = kolor /= Czarny
				mozeWykonacSkok kolor (_, _, (Damka Bialy)) = kolor /= Bialy
				mozeWykonacSkok kolor (_, _, Czarny) = kolor /= Czarny
				mozeWykonacSkok kolor (_, _, Bialy) = kolor /= Bialy
				mozeWykonacSkok _ _ = False

		-- zwraca pozycje oddalone n pol od podanej pozycji
		nPrzekatna :: Int -> Pozycja -> [Pozycja]
		nPrzekatna n (wiersz, kolumna) =
			[(wiersz+n, kolumna+n), (wiersz-n, kolumna-n), (wiersz+n, kolumna-n), (wiersz-n, kolumna+n)]

pokazSkoki :: Gra -> [Ruch]
pokazSkoki (Gra stan@(StanGry plansza _) _) =
	skoki where
		skoki = concat $ map (pokazSkokiHelper stan) pozycje
		pozycje = pozycjePlanszy plansza

		pokazSkokiHelper :: StanGry -> Pozycja -> [Ruch]
		pokazSkokiHelper (StanGry plansza@(Plansza rozmiar pozycje) gracz) pozycja =
			if (kolorPasuje gracz kolor)
				then map (Ruch pozycja) (skok plansza pozycja)
			else [] 
			where
				-- sprawdza, czy gracz ma ten sam kolor
				kolorPasuje :: Gracz -> Kolor -> Bool
				kolorPasuje (Gracz kolorGracza) kolorPozycji = (podstawowyKolor kolorGracza) == (podstawowyKolor kolorPozycji)
				kolor = kolorNaPozycji pozycje pozycja

pokazProsteRuchy :: Gra -> [Ruch]
pokazProsteRuchy (Gra stan@(StanGry plansza _) _) =
	prosteRuchy where
		prosteRuchy = concat $ map (pokazProsteRuchyHelper stan) pozycje
		pozycje = pozycjePlanszy plansza

		pokazProsteRuchyHelper :: StanGry -> Pozycja -> [Ruch]
		pokazProsteRuchyHelper (StanGry plansza@(Plansza rozmiar pozycje) gracz) pozycja =
			if (kolorPasuje gracz kolor)
				then map (Ruch pozycja) (prostyRuch plansza pozycja)
			else [] 
			where
				-- sprawdza, czy gracz ma ten sam kolor
				kolorPasuje :: Gracz -> Kolor -> Bool
				kolorPasuje (Gracz kolorGracza) kolorPozycji = (podstawowyKolor kolorGracza) == (podstawowyKolor kolorPozycji)
				kolor = kolorNaPozycji pozycje pozycja



wykonajRuch :: Gra -> Ruch -> Gra
wykonajRuch (Gra aktualnyStan gracze) ruch@(Ruch staraPozycja nowaPozycja) =
	nowaGra where
		nowaGra = Gra stan3 gracze
		stan1 = aktualizujStan aktualnyStan staraPozycja nowaPozycja
		stan2 = if sprawdzSkok ruch
			then wymazPozycje stan1 (zwrocWspolrzedneZbitegoPionka ruch)
			else stan1
		stan3 = if czyDamka (zwrocKolorStanu stan2 nowaPozycja) nowaPozycja
			then zamienNaDamke stan2 nowaPozycja
			else stan2
		-- aktualizuje stan gry wraz z wykonaniem ruchu
		aktualizujStan :: StanGry -> Pozycja -> Pozycja -> StanGry
		aktualizujStan (StanGry staraPlansza poprzedniGracz) staraPozycja nowaPozycja =
			(StanGry nowaPlansza nastepnyGracz) where
				poprzedniKolor = kolorNaPozycji (zwrocKolory staraPlansza) staraPozycja
				usunietaPozycja = aktualizujPlansze staraPlansza staraPozycja Brak
				nowaPlansza = aktualizujPlansze usunietaPozycja nowaPozycja poprzedniKolor
				nastepnyGracz = case poprzedniGracz of
					Gracz Czarny			-> Gracz Bialy
					Gracz (Damka Czarny)	-> Gracz Bialy
					_						-> Gracz Czarny
		zwrocWspolrzedneZbitegoPionka :: Ruch -> Pozycja
		zwrocWspolrzedneZbitegoPionka (Ruch (xStart, yStart) (xKoniec, yKoniec)) =
			nowaPozycja where
				nowaPozycja = (xStart+roznicaKolumn, yStart+roznicaWierszy)
				roznicaKolumn = if xStart < xKoniec
					then 1
					else -1
				roznicaWierszy = if yStart < yKoniec
					then 1
					else -1
		-- zwraca prawde, jezeli ruch jest skokiem
		sprawdzSkok :: Ruch -> Bool
		sprawdzSkok (Ruch (_, yStart) (_, yKoniec)) =
			roznica > 1 where
				roznica = abs $ yStart - yKoniec
		-- sprawdza, czy pionek na podanej pozycji powienien byc damka
		czyDamka :: Kolor -> Pozycja -> Bool
		czyDamka (Damka _) _ =
			False
		czyDamka Czarny (_, 8) =
			True
		czyDamka Bialy (_, 1) =
			True
		czyDamka _ _ =
			False
		-- zamienia pionekna podanej pozycji na damke
		zamienNaDamke :: StanGry -> Pozycja -> StanGry
		zamienNaDamke stan pozycja =
			nowyStan where
				nowyStan = zmienPozycjeStanu stan pozycja damka
				damka = rozszerzonyKolor $ zwrocKolorStanu stan pozycja
				-- zamienia noramlny kolor na damke
				rozszerzonyKolor :: Kolor -> Kolor
				rozszerzonyKolor Czarny =
					Damka Czarny
				rozszerzonyKolor Bialy =
					Damka Bialy
				rozszerzonyKolor kolor = kolor
		-- wymazuje wskazana pozycje
		wymazPozycje :: StanGry -> Pozycja -> StanGry
		wymazPozycje gra pozycja =
			zmienPozycjeStanu gra pozycja Brak

zaNieBicieTraciszZycie :: Gra -> Bool
zaNieBicieTraciszZycie gra =
	if (pokazSkoki gra) == []
		then True
	else False

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
	zwrocPlansze :: Rozmiar -> Plansza
	zwrocPlansze rozmiar =
		Plansza rozmiar (zwrocPozycjePionkow rozmiar) where
		zwrocPozycjePionkow :: Rozmiar -> PozycjePionkow
		zwrocPozycjePionkow (szerokosc, wysokosc) =
			Map.fromList $ zip pozycje (repeat Brak) where
				pozycje = [(wiersz, kolumna) | wiersz <- [1..wysokosc], kolumna <- [1..szerokosc]]
	aktualizujPozycje :: Plansza -> [Pozycja] -> Kolor -> Plansza
	aktualizujPozycje plansza pozycje kolor = nowaPlansza where
		nowaPlansza = foldr funkcja plansza pozycje
		funkcja pozycjA planszA = aktualizujPlansze planszA pozycjA kolor

graj :: Gra -> IO()
graj gra =
	do
		if not (koniecGry gra)
			then kontynuuj
			else zakoncz
		where
			mozliweProsteRuchy = 
				if zaNieBicieTraciszZycie gra
					then pokazProsteRuchy gra
				else []
			mozliweSkoki = pokazSkoki gra
			mozliweRuchy = mozliweProsteRuchy ++ mozliweSkoki
			czyPoprawnyRuch ruch = ruch `elem` mozliweRuchy
			czyPoprawnySkok skok = skok `elem` mozliweSkoki

			kontynuuj =
				do
					putStrLn $ show gra

					putStrLn $ "Wszystkie mozliwe ruchy:"
					putStrLn $ "-proste ruchy:\n" ++ show mozliweProsteRuchy
					putStrLn $ "-skoki:\n" ++ show mozliweSkoki
					putStrLn $ "Podaj swoj ruch jako: \"Ruch (poczatkowyWiersz,poczatkowaKolumna) (koncowyWiersz,koncowaKolumna)\" lub \"wyjscie\" w celu zakonczenia"
					wejscie <- getLine
					if wejscie == "wyjscie"
						then putStrLn "Koncze gre..."
					else if czyPoprawnyRuch (przekonwertuj wejscie)
						then if zaNieBicieTraciszZycie gra 
							then do
								putStrLn $ "Wykonywanie ruchu " ++ wejscie
								graj (wykonajWczytanyRuch gra wejscie)
							else if czyPoprawnySkok (przekonwertuj wejscie)
								then do
									putStrLn $ "Wykonywanie skoku " ++ wejscie
									graj (wykonajWczytanyRuch gra wejscie)
								else do
									putStrLn $ "Musisz wykonac skok. " ++ wejscie ++ " nie jest poprawnym skokiem"
									kontynuuj
						else if zaNieBicieTraciszZycie gra
							then do
								putStrLn $ wejscie ++ " nie jest poprawnym ruchem"
								kontynuuj
							else do
								putStrLn $ "Musisz wykonac skok. " ++ wejscie ++ " nie jest poprawnym skokiem"
								kontynuuj
			zakoncz =
				do
					putStrLn $ "Koniec gry. Stan koncowy:\n" ++ show gra
					putStrLn $ "Wygral: " ++ show (ktoWygral gra)	++ "\n"


przekonwertuj :: String -> Ruch
przekonwertuj wejscie = case reads wejscie of
	[(ruch, "")]	-> ruch :: Ruch		-- obluga wyjatku read'a
	_ 		-> (Ruch (0,0) (0,0))	-- w przypadku bledu podajemy niemozliwy ruch

wykonajWczytanyRuch :: Gra -> String -> Gra
wykonajWczytanyRuch gra wejscie =
	wykonajRuch gra (przekonwertuj wejscie)

main :: IO()
main =
	do
		putStrLn "Warcaby\n"
		graj poczatekGry
	
