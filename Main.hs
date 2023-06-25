import Enigma

plugboard = [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')]

enigma7 = (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
enigma2 = (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (1,1,1))


enigma3 = (SteckeredEnigma rotor2 rotor4 rotor3 reflectorB (1,4,15) [('I','W'),('G','Z'),('N','T'),('C','O'),('P','S'),('U','Y'),('Q','R'),('A','L'),('M','X')])
enigma4 = (SteckeredEnigma rotor3 rotor2 rotor1 reflectorB (0,0,25) [('C','R'),('L','N'),('E','P'),('S','I')])



trialEnigma = (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,18) [('I','W'),('Q','S'),('N','Y'),('E','O'),('L','M'),('F','H'),('D','T'),('A','Z'),('P','X')])

testEnigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (3,9,2)
p1 = "WETTERVORHERSAGEBISKAYA"
message1 = "RWIVTYRESXBFOGKUHQBAISE"
message2 = "KAZEOCEBUPDHZCDTPAZTOVI"

message3 = "WETTERVORHERSAGEBISKAYA"
x1 = encodeMessage message3 enigma2

p2 =  "THEMAINPROFESSIONALINSTITUTIONFORCOMPUTING"
m2 = "HFFJWUWFASWTBPGCVUVXWUYGORNNRSMLDWTWKMZBWT"

x2a = "QKPKFNLRTLQHVIGRIKUOEWSXRTIHWIODZORBRQJFZLMAJXKVXBPHROGJSPVIFNQERCRSZATNJXORIKPYEDBMYCLOCTHAZCSFSTGMRJCVICAJDUUETLYLFGJAFQZTDUTVPPYVROVPCGFYZTKWJEAKZZQXQCQUBMKDHYHAVIXKDLHRFCGTAADGJZLTVTWAKXHZNDFVRBUBTLWGBYXSOTVGMPSKCODKAOUCQJPJPEOIMAEMSSNVINMVVLFCMGVQCQOKAEWLVBSRBKTMXRUJOSUTCWRDHDYPHPHNVLEGVHERDSMTEUUQNRNVALCZGKRKXTNDCLLTQHVBOQYMYQMCZEZYBDKQOXRVFZBEUUYMKIYZNBDMVSFXWPUMDMGTCABLEBAPAKFQCXOPWIVDTMTIKCVIRALQKVSLVQHBFOMWRVKOWWJRMCBBQETMFBQBDCIMXMZPFJYNPVDJJFJMYGKHFDLQCYTFBXGQGRVQFKUHJQYTXQPSAJUHLSVKMJOSKRUHLDADOKQQPVEJGPWLNHYJUBDHCVBMMEPAJLUSECKYLCFMVFQLTPBYOHREQMOCDTWUXOGREDIVWJSXKHWJTKUHZHVEIIKGEEVZIMLKLMJAVYCIRWBQCSVXLTJBEXZYDJNTLWTRPNCMJWSPWQUGBUDLWQRUFYMHMNAASAHRMDREKWHTIOVZMBOTDUZHMWSMJNJIJWHEBBVGJKTHHYYCWUMTGJKROEZZKJZQDJUIOUPVIIYZIROQISYALZDHUPYTHYTLOPLFDKQZMBUOCXNOBUGGGCGQMXKUJKUUPFFGAJJIKCZRENJQLHCKTMUDFQKDZYCVGIULRKGUCOBJIGSESZXSCJYWHXGBDYGCHCIFWQYPVMBBHYNLKSZRFVYIFFNYEETKHLNLBGKWBGRIMFOVEIWHRIYQSVADSDKPYDOIKXQTUYRHPXJZFHYUQADBVLVDGMDCGLOORPZQBTVNBAPIJGSMJRHKFKSRSFSUVZDGYCSQFXDMKWKNACWXQOSQUOCJGICHNGRKIAXKQMYWHDFDEEKZJZQDBQCTAXSZYUHXLHMYJ"
x2b = "QKPKFNLRTLQHVEGRIKUOEWSXRTIHWIODZORBRQJSZLMAJXKVXBPHROGJSPVIFNQERGDDANKEGLZLUVCRHOCESEHBSQXQDZZKVMIAABJERUTULCVSZQKEZLTOMSNLIHXKVFTYPLDUNSCPCGRJGMLVOPEMKFKOWWNUAGWGEGOELYJENXUSQICSJULMANOWVWMRSXKUVCKOOILRDQTPLUMTQBMSGYUMZOMILBJRTXFMYQCVXFBTUKHCSVGSYOUICPPEOCHTPVFDMKMSQHZBEUZVIXLIMASEFVINKNDMXFKRVOPGOGWFFTLGSQPOAPENFIWOCPKTDJIBYXBZOACKOSAWUNFVEGDPAMZQMAAAZAKTHTCBOVBPUUJAYDSGJRETBUBYXLKIOSKROZLKUFIMMAMFBUZKFLGCQKJXAYJZPWUZXBGIWZNPYPELVJUSBMGFMKPMJXWOCFWQDTZTRSOBMRKKQGGQZRHVMUACCBQKBVAHOELDNTXXWFHIQOGPOFAGZUJMROBDMSMPUGMNZVSZXXVJFCNWVAMOWPVGPJGMPZHEXKHRIXIFCJMXZCPTTOYJOVNYSVWCXBBKCNVRAGJQWXYIXDAOEBQUZMTUSOBEGLKCLZEULAIBDACUXLXVYRWVQIGWDRYRSQIUUFDJUBPGGDVZHDBYQBMNWSSWLFGXMGANDUWKAWLEIKMOLNNAXKDPHNULCGFTSYYZNCOKZJRPDRYXHIQWAPLVBGQULAZPUIJSCISWPGMMUKEMYFWCKCWXCJOTRZVPFLYADFZWNMBHLZQPUAHYFGGHCCKJSSBTMPUBFWLEWNWJSDYFCKFAVUHDYYGNYADBPTFQMXJZPQTHVSYSLHSTOBWGKDYEPRFNLZJEGOKJMXDZBXRLGTGHOAIDZVTJZSPQAEHKHRBFDLOCDQOAJEYACHXMXFMFYUODNVPIJZNYVIFHZWHRJVQKGTQANDUYZTGRRFQVQMLNJDJAXSTLVIMEPYPHEZMYYKHXGSCZBCRDHXHMEGFCOTHCDVETICYWEDVPXJGJLAAOQLANLGPEUUSIKQMWXNTYHOPEG"
x2c = "QKPKFNLRTLQHVIGRIKUOEWSXRTIHWIODZORBRQJFZLMAJXKVXBPHROGJSPVIFNQERCDDANKEGLZLUVCRHOCESEHBSQXGPBLUZDVKIWNKNPQTBBSRQVBTGFTOMSNLIHXKVFTYPLDUNSCPCGRDGMLVOPEMKFKOWWNUAGWGEGOELKJENXUSQICSJULMANOWVWMRSXKPVCKOOILRDQTPLUMTQBMSGYUMZGMILBJRTXFMYQCVXFBTUKHCSVGIYOUICPPEOCHTPVFDMKMSQHZBEYZVIXLIMASEFVINKNDMXFKRVOPVOGWFFTLGSQPOAPENFIWOCPKTDDIBYXBZOACKOSAWUNFVEGDPAMZWMAAAZAKTHTCBOVBPUUJAYDSGJMETBUBYXLKIOSKROZLKUFIMMAMKBUZKFLGCQKJXAYJZPWUZXBGIWQNPYPELVJUSBMGFMKPMJXWOCFWCDTZTRSOBMRKKQGGQZRHVMUACCIQKBVAHOELDNTXXWFHIQOGPOFAUZUJMROBDMSMPUGMNZVSZXXVJFZNWVAMOWPVGPJGMPZHEXKHRIXINCJMXZCPTTOYJOVNYSVWCXBBKCYVRAGJQWXYIXDAOEBQUZMTUSOBEGLKCLZEULAIBDACUXLXVYRWVQMGWDRYRSQIUUFDJUBPGGDVZHDBKQBMNWSSWLFGXMGANDUWKAWLEIHMOLNNAXKDPHNULCGFTSYYZNCOAZJRPDRYXHIQWAPLVBGQULAZPUTJSCISWPGMMUKEMYFWCKCWXCJOPQDJBPUWVYGDVMQAXAXNACQNRHOGHCCKJSSBTMPUBFWLEWNWJSDYCCKFAVUHDYYGNYADBPTFQMXJZPETHVSYSLHSTOBWGKDYEPRFNLZJEGOKJMXDZBXRLGTGHOAIDZVTJZQPQAEHKHRBFDLOCDQOAJEYACHXRXFMFYUODNVPIJZNYVIFHZWHRJBQKGTQANDUYZTGRRFQVQMLNJDJEXSTLVIMEPYPHEZMYYKHXGSCZBSRDHXHMEGFCOTHCDVETICYWEDVNXJGJLAAOQLANLGPEUUSIKQMWXSTYHOPEG"

m3 = "NIQVDYIAJGLEOWOAPVFKXAFXNXXSBCNXTCBEBWJQDULHWRGAORIUVFQXCNYORCZIJZRWNUASAFNXAPOXOJQWUCVBCDJGLTXUXONRSJADWCEMARXAIVWEDJBCTCHDSYDYALZBAVRFGNWFYOJZZMAEECUALWLQXDBUCZBQWITZXRKHOIFPRQOJOFJZQCPFUDDCQEORIBZMMERVNFXOKUCJHHVXKGKOCNEBTQXLSFZODTUMPEMABCBJZVROPOAUFWBEXSXJNAWAOETJXRHGQTUAFRXWKGMGNJVQXWSGOCGEYQHPPNTMJFUHAAMIRVCTUUAZSVZDGDVCFNWHKVVMEWDXVLBXTWWJLZWYCPJNQZXIMTZVHDSNDQGCOUOMWXLEHUNBTBQHSHDZDEDKWAEZWBXVVZCHRJFEQZPMPVWDCXSMCIMFTKEDUISUGEUZWPFGETUBPTIQNDNMHFKUHQNXCDFSFDQOIDXVAZZGCMYYMICVDVTTFFPPRGLOCSTAEVMVMGJNZVDFXTDRIAYFXUQTAKSGNBFPVCJCGKXGBBWTVCOWPCKFFHSUZHWJHTRAOPIXYPNOSEHWRNSVHOCOOMZZLACPVJIFIKJXCFSKMWGZDJQOPZJPALGOXYQJDNNCQALOWUBAFMESIEHXLOQZQSEJFZGPVZPZOBXTTIDNZZANXRMYCRVERMCKCQDAGAMOPCTYEHQRNSUDDQNZPZNZGCZEQYEBQSQLXHIBRGPWCQDNGWSDZOLCRDCIFXHDRQDGOHSITPFKEQQDTTTBWNXFWUQEANYBGYZQSJQVHQXYYYIJMCCVRLLVQJWCEIISIMWWTCVEMYQLYOAWMPPBKLDSDSFLNIDGIYDPKNPWZLFVJBJQSEQGLDZKXZPPLFIWLSAGOEJAUTTMTWQOUFRGZZBMOXOOSRQRCLLPPUPHGVABNNVJZVEDKDSWVLWKXQDXBQMQCRZACNVMQHSVAZLWUYBFTGPQCQXYRJXRWGMEIAJWCYIPKTNBIPQXFQGJXDJKGIJMCSHJPPAIHXKJBUOMMTFJVFMYPMVLGADQLVBURBSHGPSAXRCGCBCCSKUTJLZLXRMDNYBCCEZULVCRIDNXOVTUWEPUTCGIMMMPDQKZTNYXPPCNJIHYSCGDHKNZDFJGFTJZLHFRYCKIDZMHASMSTFEPGHNIJSGRGAAXVEDVQFSVNSOTNIAJHWSJEKHTCOLXGMVNIMJFXMGGLEPFXKIEJZLWBPQLEYRJSHWGGOOGNEGAWQTXSZLCMHFCZSAVRZQMFEBEKCRIDABABBBUMECHXQJRGVLUXOEMQSMHWJXMPBQFGWPCICPFJWQCLUHBXCPXYXFVRCETZTFCNDQXPHTXWUTUGNVWTPXFLPUUGWNCZUCBGNRDPMNFQFDTTHHJHBVXTABELIPMCTISSZZQDMOCJGOUUISVCCOHUOVHQEBNBRMINIYKHWFUUSQSMNWEEFJLGTTRNIDUJRMTUSDKUNCEWGVBXHFEDJBMBSVSAFDHWXEUSZIUSHSQTNJIZWZHLRKZDRCYPIOXNATJVTISPSOAUTOFRNKNIIZXPBTOVFBNAWVICFWCOGEGBFDGYCQGHQMJEZHCDQHZXPJZHVWKJUNPYKHDGLORYDECNXGVMWODFRSTQPOGHAIGHGYHYVVMVKBWWXXPWOGUAMICUQOBQIEEDQELVMVUUPUAPECHMDJHTZEGTVUGVURZHBLSMQVWVMPZBWLZPMDWXUFQOFXRZOQQLKVYTBNCYOIEERFBXSQVZMXWOMMMAUZFJGBELVFUMVLTWGQJAUNQTABNTGGJSVAHXYVGCDFIGLIISVDYOWACALEJLSMQFVSNEHZDEJMCOBQUKQXCUDQFYGM"

mbad = "NIQVDYIAJGLEOWOAPVFKXAFXNXXSBCNXTCBEBWJQDULHWRGAORIUVFQXCNYORCZIJZRWNUASAFNXAPOXOJQWUCVBCDJGLTXUZZEDPBEZKSRUUFDGTWNAGQUKNUYXHJUTDDOVLJDUVZBRRZHLENTSHCSBQVAAVQBSGXDNWGDQGZXRIUYMPEERAKDWDIUTLOXIVXAJPNYEXOQLHLECWKXAUXOPFWCKMMHDAXJALRENHIOBMREPOZIPOEUZSTCOYBCDGVNYSZKPAIAONPTBCZEZTZOTTKLDRJPLPUIPTJVZWSFCTUHRJFRXSXGOGJCXZVOINGGNMFBIFNJHNEUHHSKRVKOSVOURUYDQLZECVZXTMTVSHZNTBURTBSFXTXNEOVGXDKRSEXSXYGGTRFDJLRJWPYUSJZHVLHTESMOLALVLKODNGJKZRJYGOXOFQJPQJZGHZMOVXCFQWRUHZYMWUSXMWOFAXGEKSBMOVARUTGKCJXFICGFMJLTKXTVGOTPCOSHQGBUYTGPUNHAWGQSXBJHSVHBRHHEPOGRQMSARYFMPTARVHLUQWBGTCJFCEGBBDTIBOIZQQRJUAJMEJJMWFUVYBESQENYXHFEGYQPKOHIBLECZQENPVNWYOPBVDQGDBKDCSPBFBUPLGJNIPLIDPHMAMHSHWGNYIZPNHPVDJTQXMDKAJPMWWQEAMBCLDNWOEHSWHRMBGDAMAGOWWNPOOAPVGRYATMHSEOUFPTOJAQZFNBSAFNVWQUXUOZGQFMUKQYBBVBOVEJNGRPUXZLJTTBQQBEERYDETTXKHRILCUDVZJLZXQNRKJGLMPMDPFYPDITHGZMBLEKNFWPXLANWSHSXCEDUPYDJGTTUKQJFZOPCQOFZWXIGBWFXSDYQUYRDRLWDIJQODWZJBGXJHFLTTVLGHKEMSGXKUZLFSHPGRLUVABFXZZCNQLGQZABBJPLIZYONCASBCFNEMTSPPYSHDHZHGLZGWXZECNUHJXOYMJFGJWYJTSAEYPJHRPAGNUMFAREXVBGDYFZUNYQFKXFMVKOVWLYYYAAAMLQSQFQTUNKKSFXJMJJEJFEKQNDMFJVOLOTFCXFHSTEAIOTEFLXFJRSNHZSQKJMUUZICHFOIMODKSDZMMWXZDLWAOPALTCFHGCZGPERIOHWIAKWRAPHEDUAVREGLQWXOULISLRREVNNHQYGLOZYBFZNLRXSFXIOYZPWRDXZNEFFZQMUUJEORUJWVFIEGXLJGKMBIDBSKHMSUDEGWJQJIAACWFUUDITVJYWHIQIEFPEPDAHRPTGMUOCHZBGQFHWZORDUJIWKZJBZWMXYAKNWZVDBWUMGMTVTQAKISPVNDCWZNPKOBMZDREHWAPAADLRMWCZPAHHNZVKXQVQSGTTZHYCXVPMDCDBCLSSRQUWTUKGLJLFPFJRELPGWHCZQFQEUAGQKBBHJHLBLZXATYGMBFVLUKWRKODYPFMQHXSCLGLCXXXDNILVBBBYQDAPOZFZJPXAMKVTEUOVWEACVSVBCAPTZSRTICENZGTBBCAKAVHCGOHLBTMHWZVOVOFHHSWFVARJGGEFVGJUXZJYGQUQPEJCEYTOPDXSIPMLEOIMGJTTIVSGOHLXSYSYSOOORERCQYQHYHITCJUKYXJTCSHYVEXAMOJHKBOUNIGFIESPTSQMWRGYYGIQSLAERWQTIQSUCMLPIUOXSYWXTCAIPQIAEKYADIDAAGLSKZVCCUSCPWPXXHYBGJFGHKEPADEULODUSHPWFHVHFLLPHAMBQTZXONDTSLVCZKZDBIPOMIXFBLHZFVOICUOFUXLHPLKBYRTDKMATETHKDHCXQRDBFCKFGPXNEIRLAPNUAPOQXI"
{- Function that will print "No result!" if Maybe type contains Nothing, or the
 - contents of the "Just" part otherwise. -}
printMaybe :: (Show a) => Maybe a -> IO ()
printMaybe = maybe (putStrLn "No result!") print

{- This is the type of thing that will happen when testing your code. Note
 - that (1) You must have your code in a module called "Enigma". (2) The functions
 - encodeMessage, longestMenu and breakEnigma are expecting arguments in a
 - particular format. Make sure you write your types so that they are compatible.
 -
 - NOTE: The actual contents of the main function when testing your code will be
 - different to this. You SHOULD NOT submit this file, only Enigma.hs.
 -}

 --"HFFJWUWFASWTBPGCVUVXWUYGORNNRSMLDWTWKMZBWT"
 --
enigma1 = (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0 ,25))

main = do
    print (encodeMessage "AAAAAAAA  AAAAAA" enigma1)

    putStrLn "First a test of encodeMessage: "

    --print(breakEnigma (zip p2 m2))

{-
    printMaybe (breakEnigma (zip p2 x2b))
    printMaybe (breakEnigma (zip p2 x2c))
    printMaybe (breakEnigma (zip x2a p2))
    printMaybe (breakEnigma (zip x2b p2))
    printMaybe (breakEnigma (zip x2c p2))
-}