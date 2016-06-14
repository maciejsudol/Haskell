import qualified Data.Map as Map	-- importujemy map i nadajemy jej kwalifikator
import Data.Map (Map)				-- tylko map
import Data.List

---Deklaracje typow------------------------------------------------

data Gracz = Gracz Kolor deriving (Show, Eq)
data Gra = Gra StanGry [Gracz] deriving (Show, Eq)
data Plansza = Plansza Rozmiar PozycjePionkow deriving (Eq)
data StanGry = StanGry Plansza Gracz deriving (Show, Eq)
data Kolor = Brak | Czarny | Bialy | Damka Kolor deriving (Show, Eq)
data Ruch = Ruch Pozycja Pozycja | ComboRuch [Ruch] deriving (Show, Eq, Read)

type Rozmiar = (Int, Int)
type Pozycja = (Int, Int)
type PozycjePionkow = Map Pozycja Kolor

---Obsluga show----------------------------------------------------

instance Show Plansza where
	show plansza@(Plansza (szerokosc, _) pozycje) =
		polacz wiersze where
			polacz = foldr (\p1 p2 -> p2 ++ ('\n':p1)) ""
			pozycjeNaPlanszy = pozycjePlanszy plansza
			pionki = concat $ map (\p -> kolorChar $ pozycje Map.! p) pozycjeNaPlanszy	-- Map.! znajduje wartość na zadanej pozycji
			wiersze = numeracjaKolumny szerokosc $ numeracjaWiersza 1 $ podzielNaKawalki (2*szerokosc) pionki
			numeracjaKolumny licznik wartosci = wartosci ++ ["  " ++ (foldr (\p1 p2 -> p2 ++ show p1 ++ " ") "" (reverse [1..licznik]))]
			numeracjaWiersza _ [] = []
			numeracjaWiersza licznik (glowa:ogon) = (show licznik ++ " " ++ glowa) : numeracjaWiersza (licznik+1) ogon
			podzielNaKawalki _ [] = []
			podzielNaKawalki dzielnik pionki = czesc1 : podzielNaKawalki dzielnik czesc2
				where
					(czesc1, czesc2) = splitAt dzielnik pionki

kolorChar :: Kolor -> String
kolorChar kolor = case kolor of
	Brak			-> "- "
	Czarny			-> "c "
	Bialy			-> "b "
	Damka Czarny	-> "C "
	Damka Bialy		-> "B "

---Funkcje glowne--------------------------------------------------

wykonajRuch :: Gra -> Ruch -> Bool -> Gra
wykonajRuch (Gra aktualnyStan@(StanGry plansza _) gracze) ruch brakZmianyGracza =
	zaktualizowanaGra where
		zaktualizowanaGra = case brakZmianyGracza of
			False	-> zaktualizowanaGraZeZmiana
			True	-> zaktualizowanaGraBezZmiany
		zaktualizowanaGraZeZmiana = case ruch of
			(Ruch staraPozycja nowaPozycja)	-> normalnyRuch
			(ComboRuch ruchy)				-> comboRuch
		zaktualizowanaGraBezZmiany = case ruch of
			(Ruch staraPozycja nowaPozycja)	-> normalnyRuchBezZmiany
			(ComboRuch ruchy)				-> comboRuchBezZmiany

		normalnyRuch :: Gra
		normalnyRuch = (Gra stan3 gracze) where
			(Ruch staraPozycja nowaPozycja) = ruch
			stan1 = aktualizujStan aktualnyStan staraPozycja nowaPozycja
			stan2 = if sprawdzBicie plansza ruch
				then wymazPozycje stan1 (zwrocWspolrzedneZbitegoPionka ruch)
				else stan1
			stan3 = if czyDamka (zwrocKolorStanu stan2 nowaPozycja) nowaPozycja
				then zamienNaDamke stan2 nowaPozycja
				else stan2

		comboRuch :: Gra
		comboRuch = (Gra stan3 gracze) where
			(ComboRuch ruchy) = ruch
			(Ruch pozycjaStartowa _) = head ruchy
			(Ruch _ pozycjaKoncowa) = last ruchy
			stan1 = aktualizujStan aktualnyStan pozycjaStartowa pozycjaKoncowa
			stan2 = foldr (\p1 p2 -> (wymazPozycje p2 (zwrocWspolrzedneZbitegoPionka p1))) stan1 ruchy
			stan3 = if czyDamka (zwrocKolorStanu stan2 pozycjaKoncowa) pozycjaKoncowa
				then zamienNaDamke stan2 pozycjaKoncowa
				else stan2

		normalnyRuchBezZmiany :: Gra
		normalnyRuchBezZmiany = (Gra stan3 gracze) where
			(Ruch staraPozycja nowaPozycja) = ruch
			stan1 = aktualizujStanBezZmiany aktualnyStan staraPozycja nowaPozycja
			stan2 = if sprawdzBicie plansza ruch
				then wymazPozycje stan1 (zwrocWspolrzedneZbitegoPionka ruch)
				else stan1
			stan3 = if czyDamka (zwrocKolorStanu stan2 nowaPozycja) nowaPozycja
				then zamienNaDamke stan2 nowaPozycja
				else stan2

		comboRuchBezZmiany :: Gra
		comboRuchBezZmiany = (Gra stan3 gracze) where
			(ComboRuch ruchy) = ruch
			(Ruch pozycjaStartowa _) = head ruchy
			(Ruch _ pozycjaKoncowa) = last ruchy
			stan1 = aktualizujStanBezZmiany aktualnyStan pozycjaStartowa pozycjaKoncowa
			stan2 = foldr (\p1 p2 -> (wymazPozycje p2 (zwrocWspolrzedneZbitegoPionka p1))) stan1 ruchy
			stan3 = if czyDamka (zwrocKolorStanu stan2 pozycjaKoncowa) pozycjaKoncowa
				then zamienNaDamke stan2 pozycjaKoncowa
				else stan2

		-- aktualizuje stan gry wraz z wykonaniem ruchu
		aktualizujStan :: StanGry -> Pozycja -> Pozycja -> StanGry
		aktualizujStan (StanGry staraPlansza poprzedniGracz) staraPozycja nowaPozycja =
			(StanGry nowaPlansza nastepnyGracz) where
				poprzedniKolor = kolorNaPozycji (zwrocPozycje staraPlansza) staraPozycja
				usunietaPozycja = aktualizujPlansze staraPlansza staraPozycja Brak
				nowaPlansza = aktualizujPlansze usunietaPozycja nowaPozycja poprzedniKolor
				nastepnyGracz = case poprzedniGracz of
					Gracz Czarny			-> Gracz Bialy
					Gracz (Damka Czarny)	-> Gracz Bialy
					_						-> Gracz Czarny

		-- nie zmienia gracza wraz z wykonaniem ruchu
		aktualizujStanBezZmiany :: StanGry -> Pozycja -> Pozycja -> StanGry
		aktualizujStanBezZmiany (StanGry staraPlansza poprzedniGracz) staraPozycja nowaPozycja =
			(StanGry nowaPlansza poprzedniGracz) where
				poprzedniKolor = kolorNaPozycji (zwrocPozycje staraPlansza) staraPozycja
				usunietaPozycja = aktualizujPlansze staraPlansza staraPozycja Brak
				nowaPlansza = aktualizujPlansze usunietaPozycja nowaPozycja poprzedniKolor

		zwrocWspolrzedneZbitegoPionka :: Ruch -> Pozycja
		zwrocWspolrzedneZbitegoPionka (Ruch (xStart, yStart) (xKoniec, yKoniec)) =
			zbitaPozycja where
				zbitaPozycja = (xKoniec+roznicaWierszy, yKoniec+roznicaKolumn)
				roznicaWierszy = if xStart < xKoniec
					then -1
					else 1
				roznicaKolumn = if yStart < yKoniec
					then -1
					else 1

		zwrocKolorStanu :: StanGry -> Pozycja -> Kolor
		zwrocKolorStanu (StanGry plansza _) pozycja =
			kolorNaPozycji (zwrocPozycje plansza) pozycja

		-- sprawdza, czy pionek na podanej pozycji powienien byc damka
		czyDamka :: Kolor -> Pozycja -> Bool
		czyDamka kolor pozycja = case (kolor, pozycja) of
			((Damka _), _)		-> False
			(Czarny, (_, 8))	-> True
			(Bialy, (_, 1))		-> True
			_					-> False

		-- zamienia pionek na podanej pozycji na damke
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

		-- zamienia dana pozycje z podanym argumentem
		zmienPozycjeStanu :: StanGry -> Pozycja -> Kolor -> StanGry
		zmienPozycjeStanu (StanGry staraPlansza gracz) pozycja kolor =
			(StanGry nowaPlansza gracz) where
				nowaPlansza = aktualizujPlansze staraPlansza pozycja kolor

-- zwraca mozliwe ruchy o jedna pozycje, bez zbijania innych pionkow
prostyRuch :: Plansza -> Pozycja -> [Pozycja]
prostyRuch plansza pozycja@(x,y) =
	filter (prawidlowaPozycja plansza) (mozliweProsteRuchy kolor) where
		kolor = kolorNaPozycji (zwrocPozycje plansza) pozycja

		mozliweProsteRuchy :: Kolor -> [Pozycja]
		mozliweProsteRuchy kolor = case kolor of
			(Damka _)	-> mozliweProsteRuchy Czarny ++ mozliweProsteRuchy Bialy
			Czarny 		-> [(x-1, y+1), (x+1, y+1)]
			Bialy 		-> [(x-1, y-1), (x+1, y-1)]
			_			-> []

		prawidlowaPozycja :: Plansza -> Pozycja -> Bool
		prawidlowaPozycja plansza pozycja =
			(pustaPozycja plansza pozycja) && (wGranicach plansza pozycja)

-- zwraca mozliwe skoki na zadanej planszy dla pionka z podanej pozycji
skok :: Plansza -> Kolor -> Pozycja -> [Pozycja]
skok plansza@(Plansza (szerokosc, wysokosc) _) kolorPionka pozycja =
	skoki where
		skoki = filter (wGranicach plansza) (mozliweSkokiFigura kolorPionka)

		mozliweSkokiFigura :: Kolor -> [Pozycja]
		mozliweSkokiFigura kolor = case kolor of
			(Damka _)	-> mozliweSkokiDamka
			Brak		-> []
			_		-> mozliweSkokiPionek

		-- zwraca mozliwe pozycje skoku dla normalnego pionka
		mozliweSkokiPionek :: [Pozycja]
		mozliweSkokiPionek =
			skoki where
				skoki = filter (pustaPozycja plansza) ruchy	-- skoki na puste pozycje
				ruchy = map (\(_, x, _) -> x) $ filter (mozeWykonacSkok kolorPionka) mozliweSkokiKrotki
				mozliweSkokiKrotki = zip3 przekatna1 przekatna2 pionkiNaPrzekatnej
				przekatna1 = nPrzekatna 1 pozycja
				przekatna2 = nPrzekatna 2 pozycja
				pionkiNaPrzekatnej = map (kolorNaPozycji (zwrocPozycje plansza)) przekatna1

				-- sprawdza, czy skok wykonywany jest nad pionkami przeciwnika
				mozeWykonacSkok :: Kolor -> (Pozycja, Pozycja, Kolor) -> Bool
				mozeWykonacSkok kolorSkaczacego kolorZbijanego = case (kolorSkaczacego, kolorZbijanego) of
					(Brak, _)											-> False
					(_, (_, _, (Damka Brak)))							-> False
					(_, (_, _, Brak))									-> False
					(kolorSkaczacego, (_, _, (Damka kolorZbijanego)))	-> kolorSkaczacego /= kolorZbijanego
					(kolorSkaczacego, (_, _, kolorZbijanego)) 			-> kolorSkaczacego /= kolorZbijanego


		-- zwraca mozliwe pozycje skoku dla damki
		mozliweSkokiDamka :: [Pozycja]
		mozliweSkokiDamka =
			skoki where
				skoki = filter (pustaPozycja plansza) (foldr (\p -> (delete p)) ruchy pozycjeDoUsuniecia)
				ruchy = filter (wGranicach plansza) (foldr (\p -> (++) (nPrzekatna p pozycja)) [] [1..szerokosc])
				pozycjePionkow = filterNot (pustaPozycja plansza) ruchy where
					filterNot predykat = filter $ not . predykat
				pozycjeTegoSamegoKoloru = filter (\p -> ((kolorNaPozycji (zwrocPozycje plansza) p) == (podstawowyKolor kolorPionka))) pozycjePionkow
				pozycjePrzeciwnegoKoloru = foldr (\p -> (delete p)) pozycjePionkow pozycjeTegoSamegoKoloru
				pozycjeZaPrzeciwnymKolorem = foldr (\p -> (++) (zaPionkiem pozycja (jednaPozycjaDalej pozycja p))) [] pozycjePrzeciwnegoKoloru
				pozycjeDoUsuniecia = (foldr (\p -> (++) (zaPionkiem pozycja p)) [] pozycjeTegoSamegoKoloru) ++ pozycjeTegoSamegoKoloru ++ pozycjeZaPrzeciwnymKolorem

		-- zwraca pozycje znajdujace sie za zadana pozycja
		zaPionkiem :: Pozycja -> Pozycja -> [Pozycja]
		zaPionkiem aktualnaPozycja pozycjaPionka =
			pozycjeZa where
				pozycjeZa = filter (naJednejProstej aktualnaPozycja pozycjaPionka) (foldr (\p -> (++) (nPrzekatna p pozycjaPionka)) [] [1..szerokosc])

				-- sprawdza, czy pionki znajduja sie na jednej przekatnej i czy pozycja znajduje sie za zadana
				naJednejProstej :: Pozycja -> Pozycja -> Pozycja -> Bool
				naJednejProstej aktualnaPozycja@(x1, y1) pozycjaPionka@(x2, y2) testowanaPozycja@(x3, y3) =
					((abs (x1-x3)) == (abs (y1-y3))) && ((roznicaWierszy > 0) && (roznicaKolumn > 0)) where
						roznicaWierszy = if x1 < x2
							then (x3-x2)
							else (x2-x3)
						roznicaKolumn = if y1 < y2
							then (y3-y2)
							else (y2-y3)

		-- zwraca o jedna pozycje dalej wgledem drugiego argumentu, dwa pierwsze argumenty znajduja sie na jednej prostej
		jednaPozycjaDalej :: Pozycja -> Pozycja -> Pozycja
		jednaPozycjaDalej aktualnaPozycja@(x1, y1) pozycjaPionka@(x2, y2) =
			(x2+roznicaWierszy, y2+roznicaKolumn) where
				roznicaWierszy = if x1 < x2
					then 1
					else -1
				roznicaKolumn = if y1 < y2
					then 1
					else -1

comboSkok :: Plansza -> Pozycja -> [([(Pozycja, Int)], Int)]
comboSkok plansza@(Plansza (szerokosc, wysokosc) _) pozycja =
	comboSkoki where
		comboSkoki = filter (\p@(pozycje, _) -> (length pozycje /= 1)) (comboSkokHelper plansza skoki pozycjeDoUsuniecia)
		ruchy = comboSkokHelper plansza skoki pozycjeDoUsuniecia
		skoki = foldr (\p -> (delete p)) (filter (sprawdzBiciePozycji pozycja) (skok plansza kolorPionka pozycja)) pozycjeDoUsuniecia
		kolorPionka = kolorNaPozycji (zwrocPozycje plansza) pozycja
		pozycjeDoUsuniecia = (filter (wGranicach plansza) (foldr (\p -> (++) (nPrzekatna p pozycja)) [] [1..szerokosc]))

		sprawdzBiciePozycji :: Pozycja -> Pozycja -> Bool
		sprawdzBiciePozycji poczatek@(x1, y1) koniec@(x2, y2) =
			((kolorNaPozycji (zwrocPozycje plansza) zbijanaPozycja) /= (kolorNaPozycji (zwrocPozycje plansza) poczatek)) && ((kolorNaPozycji (zwrocPozycje plansza) zbijanaPozycja) /= Brak) where
				zbijanaPozycja = (x2+roznicaWierszy, y2+roznicaKolumn)
				roznicaWierszy = if x1 < x2
					then -1
					else 1
				roznicaKolumn = if y1 < y2
					then -1
					else 1	

		comboSkokHelper :: Plansza -> [Pozycja] -> [Pozycja] -> [([(Pozycja, Int)], Int)]
		comboSkokHelper _ [] _ =
			[]
		comboSkokHelper plansza skoki@(head:tail) pozycjeDoUsuniecia =
			(zip [zip (head:(foldr (\p -> (delete p)) (filter (sprawdzBiciePozycji head) (skok plansza kolorPionka head)) pozycjeDoUsuniecia)) [0..]] [1..]) ++ (comboSkokHelper plansza tail (filter (wGranicach plansza) (foldr (\p -> (++) (nPrzekatna p head)) [] [1..szerokosc])))

zasugeruj :: Gra -> Ruch
zasugeruj gra@(Gra (StanGry plansza _) _) =
	najlepszyRuch where
		(najlepszyRuch, _) = head najlepszeRuchy
		wszystkieMozliweRuchy = mozliweRuchy gra
		sumyBic = zasugerujHelper gra wszystkieMozliweRuchy 3
		krotki = zip wszystkieMozliweRuchy sumyBic
		najlepszeRuchy = filter (\p@(ruch, suma) -> (suma == (maximum sumyBic))) krotki

		mozliweRuchy :: Gra -> [Ruch]
		mozliweRuchy gra =
			wszystkieMozliweRuchy where
			wszystkieMozliweRuchy = mozliweProsteRuchy ++ mozliweSkoki ++ mozliweComboSkoki
			mozliweProsteRuchy = pokazProsteRuchy gra
			mozliweSkoki = foldr (\p -> (delete p)) (pokazSkoki gra) (mozliweProsteRuchy ++ mozliweBicia)
			mozliweBicia = filter (sprawdzBicie plansza) (foldr (\p -> (delete p)) (pokazSkoki gra) (mozliweProsteRuchy))
			mozliweComboSkoki = pokazComboSkoki gra

		zasugerujHelper :: Gra -> [Ruch] -> Int -> [Int]
		zasugerujHelper _ _ 0 =
			[]
		zasugerujHelper _ [] _ =
			[]
		zasugerujHelper gra (head:tail) licznik =
			[(liczbaMozliwychBic (wykonajRuch gra head True)) + (sum (zasugerujHelper (wykonajRuch gra head True) (mozliweRuchy gra) (licznik-1)))] ++ (zasugerujHelper gra tail licznik)

		liczbaMozliwychBic :: Gra -> Int
		liczbaMozliwychBic gra@(Gra (StanGry plansza _) _) =
			(length (filter (sprawdzBicie plansza) (foldr (\p -> (delete p)) (pokazSkoki gra) (pokazProsteRuchy gra)))) + (length (pokazComboSkoki gra))

---Funkcje IO------------------------------------------------------

main :: IO()
main = do
	putStrLn "Warcaby\n"
	graj poczatekGry where

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
					generujPozycje wiersze =
						[(x, y) | x <- [1..8], y <- wiersze, or [and [even x, even y], and [odd x, odd y]]]

					zwrocPlansze :: Rozmiar -> Plansza
					zwrocPlansze rozmiar =
						Plansza rozmiar (zwrocPozycjePionkow rozmiar) where
						zwrocPozycjePionkow :: Rozmiar -> PozycjePionkow
						zwrocPozycjePionkow (szerokosc, wysokosc) =
							Map.fromList $ zip pozycje (repeat Brak) where
								pozycje = [(wiersz, kolumna) | wiersz <- [1..wysokosc], kolumna <- [1..szerokosc]]

					aktualizujPozycje :: Plansza -> [Pozycja] -> Kolor -> Plansza
					aktualizujPozycje plansza pozycje kolor = nowaPlansza where
						nowaPlansza = foldr (\p1 p2 -> aktualizujPlansze p2 p1 kolor) plansza pozycje

graj :: Gra -> IO()
graj gra@(Gra (StanGry plansza _) _) =
	do
		if not (koniecGry gra)
			then kontynuuj
			else zakoncz
		where
			mozliweProsteRuchy = pokazProsteRuchy gra
			mozliweSkoki = foldr (\p -> (delete p)) (pokazSkoki gra) (mozliweProsteRuchy ++ mozliweBicia)
			mozliweBicia = filter (sprawdzBicie plansza) (foldr (\p -> (delete p)) (pokazSkoki gra) (mozliweProsteRuchy))
			mozliweComboSkoki = pokazComboSkoki gra
			mozliweRuchy = mozliweProsteRuchy ++ mozliweSkoki ++ mozliweBicia ++ mozliweComboSkoki

			czyPoprawnyRuch ruch =
				ruch `elem` mozliweRuchy
			czyPoprawnySkok skok =
				skok `elem` mozliweSkoki
			czyPoprawneBicie bicie =
				bicie `elem` mozliweBicia
			czyPoprawnyComboSkok comboSkok =
				comboSkok `elem` mozliweComboSkoki

			zaNieBicieTraciszZycie :: Gra -> Bool
			zaNieBicieTraciszZycie gra =
				if (mozliweBicia == []) && (mozliweComboSkoki == [])
					then True
				else False

			przekonwertuj :: String -> Ruch
			przekonwertuj wejscie = case reads wejscie of
				[(ruch, "")]	-> ruch :: Ruch		-- obluga wyjatku read'a
				_ 		-> (Ruch (0,0) (0,0))		-- w przypadku bledu podajemy niemozliwy ruch

			wykonajWczytanyRuch :: Gra -> String -> Gra
			wykonajWczytanyRuch gra wejscie =
				wykonajRuch gra (przekonwertuj wejscie) False

			kontynuuj =
				do
					putStrLn $ show gra

					putStrLn $ "Wszystkie mozliwe ruchy:"
					putStrLn $ " -proste ruchy:\n" ++ show mozliweProsteRuchy
					putStrLn $ " -skoki:\n" ++ show mozliweSkoki
					putStrLn $ " -bicia:\n" ++ show mozliweBicia
					putStrLn $ " -combo skoki:\n" ++ show mozliweComboSkoki
					putStrLn $ " -sugerowany najlepszy ruch:\n" ++ 
						if (length mozliweBicia /= 0) || (length mozliweComboSkoki /= 0)
							then "Musisz wykonac bicie"
						else show (zasugeruj gra)
					putStrLn $ "Podaj swoj ruch jako: \"Ruch (poczatkowyWiersz,poczatkowaKolumna) (koncowyWiersz,koncowaKolumna)\" lub \"ComboRuch [<ruchy>]\" w przypadku combo skoku. W celu wyjscia wpisz \"wyjscie\""
					wejscie <- getLine
					if wejscie == "wyjscie"
						then putStrLn "Koncze gre..."
					else if czyPoprawnyRuch (przekonwertuj wejscie)
						then if zaNieBicieTraciszZycie gra 
							then do
								putStrLn $ "Wykonywanie ruchu " ++ wejscie
								graj (wykonajWczytanyRuch gra wejscie)
						else if (czyPoprawneBicie (przekonwertuj wejscie)) || (czyPoprawnyComboSkok (przekonwertuj wejscie))
							then do
								putStrLn $ "Wykonywanie bicia/combo skoku " ++ wejscie
								graj (wykonajWczytanyRuch gra wejscie)
							else do
								putStrLn $ "Musisz wykonac bicie lub combo skok. Wejscie: " ++ wejscie ++ " nie jest poprawne"
								kontynuuj
					else if zaNieBicieTraciszZycie gra
						then do
							putStrLn $ wejscie ++ " nie jest poprawnym ruchem"
							kontynuuj
						else do
							putStrLn $ "Musisz wykonac bicie lub combo skok. Wejscie: " ++ wejscie ++ " nie jest poprawne"
							kontynuuj

			zakoncz =
				do
					putStrLn $ "Koniec gry!!!\n" ++ show gra
					putStrLn $ "Wygral " ++ show (ktoWygral gra) ++ "\n"

---Funkcje pomocnicze----------------------------------------------

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

zwrocPozycje :: Plansza -> PozycjePionkow
zwrocPozycje (Plansza _ pozycje) =
	pozycje

-- aktualizuje kolory
zamienKolory :: Plansza -> PozycjePionkow -> Plansza
zamienKolory (Plansza rozmiar _) noweKolory =
	(Plansza rozmiar noweKolory)

zamienKolor :: Kolor -> Kolor
zamienKolor kolor = case kolor of
	(Damka kolorDamki)	-> zamienKolor kolorDamki
	Bialy				-> Czarny
	Czarny				-> Bialy
	_					-> kolor

-- aktualizuje pozycje pionka na planszy
aktualizujPlansze :: Plansza -> Pozycja -> Kolor -> Plansza
aktualizujPlansze plansza pozycja kolor =
	nowaPlansza where
		stareKolory = zwrocPozycje plansza
		noweKolory = Map.insert pozycja kolor stareKolory
		nowaPlansza = zamienKolory plansza noweKolory

-- zwraca prawde, jezeli ruch jest biciem
sprawdzBicie :: Plansza -> Ruch -> Bool
sprawdzBicie plansza (Ruch poczatek@(x1, y1) koniec@(x2, y2)) =
	((kolorNaPozycji (zwrocPozycje plansza) zbijanaPozycja) /= (kolorNaPozycji (zwrocPozycje plansza) poczatek)) && ((kolorNaPozycji (zwrocPozycje plansza) zbijanaPozycja) /= Brak) where
		zbijanaPozycja = (x2+roznicaWierszy, y2+roznicaKolumn)
		roznicaWierszy = if x1 < x2
			then -1
			else 1
		roznicaKolumn = if y1 < y2
			then -1
			else 1	

koniecGry :: Gra -> Bool
koniecGry (Gra (StanGry plansza _) _) =
	any (wygral plansza) [Bialy, Czarny] where
		wygral :: Plansza -> Kolor -> Bool
		wygral _ Brak = False
		wygral plansza (Damka kolor) = wygral plansza kolor
		wygral (Plansza _ pozycje) kolor =
			not istniejeInnyKolor where
				istniejeInnyKolor = any (funkcja innyKolor) (Map.elems pozycje)
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

-- sprawdza, czy zadana pozycja jest pusta
pustaPozycja :: Plansza -> Pozycja -> Bool
pustaPozycja plansza pozycja =
	(kolorNaPozycji (zwrocPozycje plansza) pozycja) == Brak

-- sprawdza, czy nowa zadana pozycja jest prawidlowa
wGranicach :: Plansza -> Pozycja -> Bool
wGranicach plansza@(Plansza (szerokoscPlanszy, wysokoscPlanszy) _) pozycja@(x, y) =
	((x > 0) && (x <= szerokoscPlanszy)) && ((y > 0) && (y <= wysokoscPlanszy))

-- zwraca pozycje oddalone n pol od podanej pozycji
nPrzekatna :: Int -> Pozycja -> [Pozycja]
nPrzekatna n (wiersz, kolumna) =
	[(wiersz+n, kolumna+n), (wiersz-n, kolumna-n), (wiersz+n, kolumna-n), (wiersz-n, kolumna+n)]			

pokazComboSkoki :: Gra -> [Ruch]
pokazComboSkoki (Gra stan@(StanGry plansza _) _) =
	skoki where
		skoki = concat $ map (\p -> (pokazComboSkokiHelper stan p p)) pozycje
		pozycje = pozycjePlanszy plansza

		pokazComboSkokiHelper :: StanGry -> Pozycja -> Pozycja -> [Ruch]
		pokazComboSkokiHelper (StanGry plansza@(Plansza rozmiar pozycje) gracz) pozycjaStartowa pozycja =
			if (kolorPasuje gracz kolor)
				then map (\p1@(lista, licznikGlowny)-> (ComboRuch (map (\p2@(aktualnaPozycja, licznikPoboczny) -> (Ruch (poprzedniaPozycja licznikGlowny licznikPoboczny) aktualnaPozycja)) lista))) (comboSkok plansza pozycja)
			else [] where
				-- sprawdza, czy gracz ma ten sam kolor
				kolorPasuje :: Gracz -> Kolor -> Bool
				kolorPasuje (Gracz kolorGracza) kolorPozycji =
					(podstawowyKolor kolorGracza) == (podstawowyKolor kolorPozycji)
				kolor = kolorNaPozycji pozycje pozycja

				poprzedniaPozycja :: Int -> Int -> Pozycja
				poprzedniaPozycja licznikGlowny licznikPoboczny =
					if licznikPoboczny == 0
						then pozycjaStartowa
					else poprzedniaPozycja where
						comboRuch@(lista, _) = last (take licznikGlowny (comboSkok plansza pozycja))
						(poprzedniaPozycja, _) = last (take licznikPoboczny lista)

pokazSkoki :: Gra -> [Ruch]
pokazSkoki (Gra stan@(StanGry plansza _) _) =
	skoki where
		skoki = concat $ map (pokazSkokiHelper stan) pozycje
		pozycje = pozycjePlanszy plansza

		pokazSkokiHelper :: StanGry -> Pozycja -> [Ruch]
		pokazSkokiHelper (StanGry plansza@(Plansza rozmiar pozycje) gracz) pozycja =
			if (kolorPasuje gracz kolor)
				then map (Ruch pozycja) (skok plansza kolorPionka pozycja)
			else [] where
				kolorPionka = kolorNaPozycji (zwrocPozycje plansza) pozycja
				-- sprawdza, czy gracz ma ten sam kolor
				kolorPasuje :: Gracz -> Kolor -> Bool
				kolorPasuje (Gracz kolorGracza) kolorPozycji =
					(podstawowyKolor kolorGracza) == (podstawowyKolor kolorPozycji)
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
			else [] where
				-- sprawdza, czy gracz ma ten sam kolor
				kolorPasuje :: Gracz -> Kolor -> Bool
				kolorPasuje (Gracz kolorGracza) kolorPozycji =
					(podstawowyKolor kolorGracza) == (podstawowyKolor kolorPozycji)
				kolor = kolorNaPozycji pozycje pozycja
