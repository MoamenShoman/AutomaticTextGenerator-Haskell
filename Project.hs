import DataFile

wordTokenHelper "" acc = [acc]
wordTokenHelper (x:xs) acc|(x == ' ') = acc:wordTokenHelper xs ""
				    |(elem x punct) = acc:wordTokenHelper xs [x]
				    |otherwise = wordTokenHelper xs (acc ++ [x])

wordToken s = wordTokenHelper s ""

wordTokenList l = foldr (++) [] (map wordToken l)


uniqueBigrams list = uniqueBigramsHelper list []

uniqueBigramsHelper (x:[]) acc = acc
uniqueBigramsHelper (x1:x2:xs) acc = if elem (x1,x2) acc then uniqueBigramsHelper (x2:xs) acc
                                     else  uniqueBigramsHelper (x2:xs) (acc ++[(x1,x2)])
uniqueTrigrams list = uniqueTrigramsHelper list []
uniqueTrigramsHelper (x1:x2:[]) acc = acc
uniqueTrigramsHelper (x1:x2:x3:xs) acc = if elem (x1,x2,x3) acc then uniqueTrigramsHelper (x2:x3:xs) acc
                                         else uniqueTrigramsHelper (x2:x3:xs) (acc ++[(x1,x2,x3)])

bigramsFreq list = bigramFreqHelper list []
bigramFreqHelper (x1:[]) acc = acc
bigramFreqHelper (x1:x2:xs) acc | search (x1,x2) acc =
                                    bigramFreqHelper (x2:xs) (inc (x1,x2) acc)
                                |otherwise = bigramFreqHelper (x2:xs) (((x1,x2),1):acc)



search (x1,x2) [] = False
search (x1,x2) (((y1,y2),y3):ys) |(x1 == y1 && x2 == y2) = True
                               |otherwise = search (x1,x2) ys

inc (x1,x2) [] = []
inc (x1,x2) (((y1,y2),occ):ys) | (x1 == y1 && x2 == y2) = ((y1,y2),occ+1):ys
                               | otherwise = ((y1,y2),occ):(inc (x1,x2) ys)






trigramsFreq list = trigamsFreqHelper list []
trigamsFreqHelper (x1:x2:[]) acc = acc
trigamsFreqHelper (x1:x2:x3:xs) acc | search2 (x1,x2,x3) acc = trigamsFreqHelper (x2:x3:xs) (inc2 (x1,x2,x3) acc)
                                    | otherwise = trigamsFreqHelper (x2:x3:xs) (((x1,x2,x3),1):acc)



search2 (x1,x2,x3) [] = False
search2 (x1,x2,x3) (((y1,y2,y3),occ):ys) | (x1 == y1 && x2 == y2 && x3 == y3) = True
                               					 | otherwise = search2 (x1,x2,x3) ys

inc2 (x1,x2,x3) [] = []
inc2 (x1,x2,x3) (((y1,y2,y3),occ):ys) | (x1 == y1 && x2 == y2 && x3 == y3) = ((y1,y2,y3),occ+1):ys
                               				| otherwise = ((y1,y2,y3),occ):(inc2 (x1,x2,x3) ys)

getFreq x [] =0
getFreq x ((x1,y1):xs)| (x==x1) = y1
                      |otherwise = getFreq x xs

generateOneProb ((x1,x2,x3),occx) list = occx/(getFreq (x1,x2) list)


genProbPairshelper list ((x1,x2,x3),occx) = ((x1,x2,x3),occx/(getFreq (x1,x2) list))
genProbPairs listTri listBi = map (genProbPairshelper listBi) listTri




generateNextWordHelper [x1,x2] ((y1,y2,y3),prob) = x1==y1 && x2==y2 && prob >= 0.03
generateNextWord[x1, x2] list|(length(filter (generateNextWordHelper [x1,x2]) list) == 0) =
                              error "Sorry, it is not possible to infer from current database"
generateNextWord [x1,x2] list =  getThird ((result) !! idx)
                                where (result, idx) = (filter (generateNextWordHelper [x1,x2]) list, randomZeroToX ((length result) - 1))

getThird ((x1, x2, x3), prob) = x3



generateTextHelper list triProb n|((n+2)==length list)=list
generateTextHelper list triProb n = generateTextHelper (list ++ [generateNextWord ([list !!((length list) -2),list !!((length list) -1)]) triProb]) triProb n



generateText s n =  foldr1 makeString ans
			where (list,triProb,ans) = (wordToken s, genProbPairs (trigramsFreq (wordTokenList docs)) (bigramsFreq (wordTokenList docs)),generateTextHelper list triProb n)


makeString s1 s2 |(elem (s2 !! 0) punct) = (s1 ++ s2)
		             |otherwise = (s1 ++ " " ++ s2)
