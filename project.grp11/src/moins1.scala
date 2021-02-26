import java.util.function.IntBinaryOperator

object moins1 extends App {

  import com.tncy.top.image.ImageWrapper;

  var imageBase: String = "Image/imageEtape-1.png";
  var imageCachee: String = "Image/imageEtape-1hidden.png";
  var outputFile: String = "Image/ImageSortie.png"
  var outputFileStage2: String = "Image/ImageEtape2.png"
  var outputFileDecoder: String = "Image/ImageDecoder.png"
  var outputFileEtape2ter: String = "Image/ImageEtape2ter.png"

  var wrappedBase: ImageWrapper = new ImageWrapper(imageBase);
  var wrappedCachee: ImageWrapper = new ImageWrapper(imageCachee);
  var wrappedImageCoder: ImageWrapper = new ImageWrapper(outputFile);
  var wrappedImageDecoder: ImageWrapper = new ImageWrapper(outputFileDecoder);
  var wrappedImageEtape2ter: ImageWrapper = new ImageWrapper(outputFileEtape2ter);

  var matBase: Array[Array[Int]] = wrappedBase.getImage();
  var matCachee: Array[Array[Int]] = wrappedCachee.getImage();
  var matImageCoder: Array[Array[Int]] = wrappedImageCoder.getImage();
  var matImageDecoder: Array[Array[Int]] = wrappedImageDecoder.getImage();

  var pixelBaseSave: Int = 0;
  var pixelCacheeSave: Int = 0;
  var newpixelImage: Int = 0;
  var TauxDivision:Array[Int]=Array(8,8);

  //Etape -1 : Fonction cache l'image -------------------------------------------------------------------------------------------------------------------------------------------------------------------
  def cachee(Base: ImageWrapper, Cachee: ImageWrapper) {
    //entrée : ImageWrapper de l'image à cacher et de l'image dans laquelle on cache
    //sortie : nouvelle image contenant l'image cachée

    var matBase: Array[Array[Int]] = Base.getImage();
    var matCachee: Array[Array[Int]] = Cachee.getImage();

    // Initialisation de la matrice de la nouvelle image

    for (i <- 0 until matBase.length) {
      for (j <- 0 until matBase(0).length) {

        //On selectionne les pixels de l'image de base pour laisser de la place pour l'image a cacher.

        pixelBaseSave = ((matBase(i)(j)) & 0xf0f0f0f0)

        //On selectionne les bits que l'on veut (les 4 premiers) puis on fait un decalage de 4 bits,
        // or on apres le decalage, une serie de 4 "1" au debut,
        // donc on effectue encore une autre operation and pour les enlever"

        pixelCacheeSave = (((matCachee(i)(j)) & 0xf0f0f0f0) >> 4) & 0x0fffffff;


        //On "regroupe" les deux images

        newpixelImage = pixelCacheeSave + pixelBaseSave;

        //On remplace par le nouveau pixel constitue de l'image de base et de l'image cachee

        matBase(i)(j) = newpixelImage;
      }
    }

    Base.saveImage(outputFile)
  }


  // Etape 0 : Fonction pour retrouver l'image cachee --------------------------------------------------------------------------------------------------------------------------------------------------
  def retrouve(ImageCoder: ImageWrapper) {
    //entrée : imageWrapper contenant une image cachée
    //sortie : image retrouvée dans l'image d'entrée

    var matImageCoder: Array[Array[Int]] = ImageCoder.getImage();

    for (i <- 0 until  matImageCoder.length) {
      for (j <- 0 until matImageCoder(0).length) {
        //Extraction des bits de l'image cachee
        newpixelImage = (((matImageCoder(i)(j)) & 0x0f0f0f0f) << 4) & 0xfffffff0;
        matImageCoder(i)(j) = newpixelImage
      }
    }
    ImageCoder.saveImage(outputFileDecoder)
  }

  //cachee(wrappedBase,wrappedCachee)
  //retrouve(wrappedImageCoder)


  // Etape 1 : fonction pour determiner la perte de qualite -----------------------------------------------------------------------------------------------------------------------------------
  //Fonction EQM (...) est une premiere mesure qunatitatif de la qualite de l'image decoder par rapport a l'image de base
  def EQM(wrappedCachee : ImageWrapper, wrappedImageDecoder: ImageWrapper): Float = { //Mettre des matrices directement on l'a dans la variable
    var eqm : Float = 0;
    var matCachee = wrappedCachee.getImage();
    var matImageDecoder = wrappedImageDecoder.getImage();

    for (i <- 0 to wrappedCachee.width - 1) {
      for (j <- 0 to wrappedCachee.height - 1) {
        var pixelSource = matCachee(j)(i);
        var pixelCompare = matImageDecoder(j)(i);
        eqm = eqm + Math.pow(pixelSource-pixelCompare,4).toLong
      }
    }
    eqm = math.sqrt(math.sqrt(eqm)).toFloat
    println(eqm)
    return eqm;
  }


  //Conversion Hexadecimal en nombre entier
  def decompoHexa(motHexa:String): Int ={
    //entrée : chaîne de caractères en hexadecimal
    //sortie : conversion en decimal
    val number = Integer.parseInt(motHexa, 16) //pour un mot en hexadecimal, il est converti en décimal par la méthode parseInt
    return number
  }


  def nivGris(matImage:Array[Array[Int]]):Array[Array[Float]]={
    //entrée : matrice d'une image
    //sortie : matrice contenant les valeurs de luminance pour chaque pixel de cette image
    //création d'une nouvelle matrice correspondant à l'image en niveau de gris (utile pour le calcul de SSIM)
    var matGris = Array.ofDim[Float](matImage.length,matImage(0).length) //création de la nouvelle matrice, vide, de mêmes dimensions que la matrice de l'image
    for (i <- 0 until matImage.length ) {
      for (j <- 0 until matImage(0).length ) {
        val Pix = matImage(i)(j); //on récupère l'entier qui correspond au codage des couleurs pour un pixel

        val R : String = (Pix.toHexString.slice(2,4)); //on convertit cet entier en hexadecimal et on récupère les 2 lettres correspondant à l'octet de codage du Rouge
        val Ri : Int = decompoHexa(R);//conversion des deux lettres hexa en décimal (pour le calcul)
        //on fait de même pour le vert et le bleu
        val G : String = (Pix.toHexString.slice(4,6));
        val Gi : Int = decompoHexa(G);

        val B : String = (Pix.toHexString.slice(6,8));
        val Bi : Int = decompoHexa(B);

        var moy : Float = (0.2126*Ri+0.7152*Gi+0.0722*Bi).toFloat;//calcul de luminance à l'aide de la formule de la recommandation 709
        matGris(i)(j) = math.round(moy);//on ajoute la valeur de luminance du pixel dans la matrice
      }
    }
    return matGris
  }



  def calculs(imageGrise:Array[Array[Float]]):(Float,Float)= {
    //entrée : matrice d'une image déjà convertie en nuances de gris avec nivGris
    //sortie : doublet de flottants correspondant à (moyenne de luminance,écart-type)
    var u : Float = 0 //u sera la luminance moyenne
    var o : Float = 0 //o sera l'écart-type
    //calcul de u
    for (i <- 0 until imageGrise.length ) {
      for (j <- 0 until imageGrise(0).length) {
        u = u + imageGrise(i)(j)
      }
    }
    //affectation de u dans le cas où la moyenne est nulle (zone noire)
    if (u == 0){
      u=0
    }
    //affectation de u dans le cas où u est non nul
    else {
      u = (u / ((imageGrise.length) * (imageGrise(0).length)))
    }

    //calcul de o
    for (i <- 0 until imageGrise.length) {
      for (j <- 0 until imageGrise(0).length) {
        o = o + math.pow(imageGrise(i)(j)-u,2).toFloat
      }
    }
    o=math.sqrt((o/(imageGrise.length*imageGrise(0).length-1))).toFloat
    return (u,o)
  }


  def sigmaXY(matX:Array[Array[Float]], matY:Array[Array[Float]]):Float={
    //entrée : matX est la matrice extraite de l'image cachée d'origine, convertie en nuances de gris
    //         matY est la matrice extraite de l'image décodée, convertie en nuances de gris
    //sortie : valeur du sigmaXY dont il est question dans la formule de SSIM
    var s : Float =0
    val ux = calculs(matX)._1 //la fonction calculs renvoie un doublet de flottants, on récupère ici le premier, qui est la moyenne de luminance sur matX
    val uy = calculs(matY)._1 //idem avec matY
    for (i <- 0 until matX.length) {
      for (j <- 0 until matX(0).length) {
        val xk : Float = matX(i)(j)
        val yk : Float = matY(i)(j)
        s = s + (xk-ux)*(yk-uy) //calcul d'après la formule de sigmaXY
      }
    }
    s = (s/(matX.length*matX(0).length-1))
    return (s)
  }

  //Création de sous-matrice de dimension 2 pour la fonction SSIM
  def subArrayDim2(mat:Array[Array[Float]],min_x:Int,max_x:Int,min_y:Int,max_y:Int):Array[Array[Float]]={
    //entrée : matrice dune image déjà convertie en nuances de gris avec nivGris
    //         min_x : abscisse minimale de la zone à extraire
    //         max_x : abscisse maximale
    //         min_y : ordonnée minimale
    //         max_y : ordonnée maximale
    //sortie : matrice de la zone demandée, délimitée par min_x, max_x, min_y, max_y

    var subArray : Array[Array[Float]] = Array.ofDim[Float](max_x-min_x+1,max_y-min_y+1) //On crée la "base" de la sous-matrice

    //On remplit la matrice ligne par ligne
    for (y <- 0 until max_x-min_x){
      subArray(y) = mat(y+min_x).slice(min_y,max_y+1)
    }
    return subArray
  }


  //Calcul de MSSIM
  def MSSIM(mat:Array[Array[Int]],matdecoder:Array[Array[Int]]):Float={
    //entrée : matrices complètes des images à comparer avec la méthode SSIM
    //sortie : valeur calculée avec la méthode

    //conversion des matrices en niveau de gris
    var matgris : Array[Array[Float]] = nivGris(mat);
    var matgrisdecoder : Array[Array[Float]] = nivGris(matdecoder)
    //nombre de zones que l'on va avoir dans le découpage de l'image sur la longueur (quotient_x) et la largeur (quotient_y)
    var quotient_x : Int = matgris(0).length/TauxDivision(0);
    var quotient_y : Int = matgris.length/TauxDivision(1);
    //reste des divisions eucliiednnes pour avoir les cases restantes
    var reste_x : Int= matgris(0).length%TauxDivision(0);
    var reste_y : Int = matgris.length%TauxDivision(1);
    //compteur du nombre de sub-divisions (nombre de rectangles) pour faire la moyenne totale de SSIM
    var nbr_element : Int = 0;
    //La somme des valeurs de SSIM
    var somme : Float = 0;

    //On calcule SSIM sur des portions choisies en fonction du Taux de Division
    //On a 4 types de "zones" ou rectanglse dans lesquelles nous allons calculer SSIM

    //Les premiers rectangles dans la parties en haut à gauche
    for (i <- 1 until TauxDivision(1)) {
      for (j <- 1 until TauxDivision(0)) {
        var testSSIM1: Float = SSIM(subArrayDim2(matgris, (j - 1) * quotient_x, j * quotient_x, (i - 1) * quotient_y, i * quotient_y), subArrayDim2(matgrisdecoder, (j - 1) * quotient_x, j * quotient_x, (i - 1) * quotient_y,i*quotient_y))
        if (testSSIM1 isNaN) {
          testSSIM1 = 0
        }
        somme = somme + testSSIM1
        nbr_element = nbr_element + 1
      }
    }

    //Le reste des pixels qui n'a pas ete compté tout à droite de l'image (sur toute la largeur)
    if (reste_x!=0){
      for (i <- 1 to TauxDivision(1)){
        somme = somme + SSIM(subArrayDim2(matgris,matgris.length - reste_x,matgris.length,(i-1)*quotient_y,i*quotient_y),subArrayDim2(matgrisdecoder,matgris.length - reste_x,matgris.length,(i-1)*quotient_y,i*quotient_y));
        nbr_element = nbr_element +1;
      }
    }

    //Le reste des pixels qui n'a pas été compté tout en bas de l'image (sur toute la longueur)
    if (reste_y!=0){
      for (j <- 1 to TauxDivision(0)){
        somme = somme + SSIM(subArrayDim2(matgris,(j-1)*quotient_x,j*quotient_x,(mat(0).length-reste_y),mat(0).length),subArrayDim2(matgrisdecoder,(j-1)*quotient_x,j*quotient_x,(mat(0).length-reste_y),mat(0).length))
        nbr_element = nbr_element +1;
      }
    }

    //Dernier rectangle en bas à droite
    if (reste_x!=0 && reste_y!=0) {
      somme = somme + SSIM(subArrayDim2(matgris,mat(0).length - reste_y, mat(0).length,(mat.length - reste_x), mat.length),subArrayDim2(matgrisdecoder,mat(0).length - reste_y, mat(0).length,(mat.length - reste_x), mat.length));
      nbr_element = nbr_element +1 ;
    }

    var MSSIM : Float = (somme/nbr_element).toFloat;

    println(MSSIM)
    return MSSIM
  }

  def SSIM(sousMatX:Array[Array[Float]], sousMatY:Array[Array[Float]]):Float= {
    //entrée : matX = matrice d'une zone de l'image cachée d'origine
    //         matY = matrice de cette même zone dans l'image décodée
    //sortie : valeur de SSIM pour la zone d'image demandée
    val L = 255 //plage de valeurs pour chaque sous pixel : de 0 à 255
    val K1 = 0.01 //valeur constante choisie arbitrairement à 0.01 d'après l'article de Zhou Wang
    val K2 = 0.02 //valeur constante choisie arbitrairement à 0.02 d'après l'article de Zhou Wang
    val C1 = (L*K1)*(L*K1) //calcul des constantes C1 et C2
    val C2 = (L*K2)*(L*K2)
    val muX = calculs(sousMatX)._1 //on récupère le premier flottant renvoyé par calculs, qui est la moyenne de luminance sur sousMatX
    val muY = calculs(sousMatY)._1 //on récupère le premier flottant renvoyé par calculs, qui est la moyenne de luminance sur sousMatY
    val sigX = calculs(sousMatX)._2 //on récupère le premier flottant renvoyé par calculs, qui est le sigmaX sur sousMatX
    val sigY = calculs(sousMatY)._2 //on récupère le premier flottant renvoyé par calculs, qui est le sigmaY sur sousMatY
    val sigXY = sigmaXY(sousMatX,sousMatY)
    val SSIMXYnum = tronc((math.log(((2*muX*muY+C1)*(2*sigXY+C2)))).toFloat,3)
    val SSIMXYdenom = tronc((math.log((muX*muX+muY*muY+C1)*(sigX*sigX+sigY*sigY+C2))).toFloat,3) //calcul de SSIM à partir de la formule établie dans l'article
    return((math.exp(SSIMXYnum-SSIMXYdenom)).toFloat)
  }

  def tronc(x:Float,i:Int):Float={
    //entrée : x un flottant
    //         i le nombre de décimales que l'on veut garder
    //sortie : flottant x avec i décimales
    return (((math floor x * math.pow(10,i))/math.pow(10,i)).toFloat)
  }


//Etape 2 -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  def HexaToBin(mot:String):String={
    //entrée : chaîne de caractères en hexadécimal
    //sortie : chaîne de caractères convertie en binaire
    var deci = decompoHexa(mot)
    var bin = deci.toBinaryString
    return bin
  }

  def BinToInt(mot:String):Int={
    //entrée : chaîne de caractères en binaire
    //sortie : entier correspondant à la valeur du mot d'entrée
    var res : Int = 0
    var L = mot.size
    //on réalise la somme des puissances de 2 correspondant au rang des "1" dans le mot d'entrée
    for (i <- 0 to L-1){
      res = res + math.pow(2,L-i-1).toInt*mot(i).toString.toInt
    }
    return(res+1)
  }

  def etape2(Base:ImageWrapper,Cachee:ImageWrapper,T:Int,R:Int,V:Int,B:Int){
    //entrée : ImageWrapper de l'image qui sera cachée et de celle qui servira de support
    //         T, R, V, B : entiers correspondant au nombre de bits que l'on souhaite changer dans l'image support pour cacher l'autre image
    //cette fonction enregistre l'image résultante en écrasant l'image "Image/ImageEtape2.png"

    //création des matrices des images d'entrée
    var matBase: Array[Array[Int]] = Base.getImage();
    var matCachee: Array[Array[Int]] = Cachee.getImage();

    //cas où les nombres demandés en entrée sont supérieurs au nombre de bits disponibles
    if (T<0 || T>8 || R<0 || R>8 || V<0 || V>8 ||B<0 || B>8) {
      println("Un de vos quatre nombres n'est pas accepté, veuillez réessayer.")
    }
    //cas où les entrées sont conformes
    else {
      for (i <- 0 until matBase.length) {
        for (j <- 0 until matBase(0).length) {
          //création de la chaîne de caractères binaire contenant les informations du pixel de chaque image
          var pixelBase: String = matBase(i)(j).toBinaryString
          var pixelCachee: String = matCachee(i)(j).toBinaryString
          //séparation des chaînes correspondant à la transparence
          var tb: String = pixelBase.slice(0, 8)
          var tc: String = pixelCachee.slice(0, 8)
          //séparation des chaînes correspondant au rouge
          var rb: String = pixelBase.slice(8, 16)
          var rc: String = pixelCachee.slice(8, 16)
          //séparation des chaînes correspondant au vert
          var vb: String = pixelBase.slice(16, 24)
          var vc: String = pixelCachee.slice(16, 24)
          //séparation des chaînes correspondant au bleu
          var bb: String = pixelBase.slice(24, 32)
          var bc: String = pixelCachee.slice(24, 32)

          //sélection des bits que l'on veut utiliser sur la transparence
          var tbFIN = tb.slice(0, 8 - T)
          var tcFIN = tc.slice(8 - T, 8)
          var tFIN = tbFIN + tcFIN
          //...le rouge
          var rbFIN = rb.slice(0, 8 - R)
          var rcFIN = rc.slice(8 - R, 8)
          var rFIN = rbFIN + rcFIN
          //...le vert
          var vbFIN = vb.slice(0, 8 - V)
          var vcFIN = vc.slice(8 - V, 8)
          var vFIN = vbFIN + vcFIN
          //...le bleu
          var bbFIN = bb.slice(0, 8 - B)
          var bcFIN = bc.slice(8 - B, 8)
          var bFIN = bbFIN + bcFIN

          //assemblage des 4 chaînes de caractères pour former le nouveau mot binaire du pixel
          var newpixelImage1 = tFIN + rFIN + vFIN + bFIN
          //remplacement du pixel de la matrice par le nouveau (que l'on convertit en entier pour reformer l'image)
          matBase(i)(j) = BinToInt(newpixelImage1)
        }
      }
      Base.saveImage(outputFileStage2)
    }
  }


  // Etape 2bis : On cherche la quantite d'information maximal stockable --------------------------------------------------------------------------------------------------------------------------------------------

  def tailleMaxMessage (Erreur_Max:Double, mat:Array[Array[Int]]): Int={
    //entrée : L'erreur maximale autorisée et la matrice concernée
    //sortie : Le nombre de bit pouvant etre libéré(s) tout en ne pas dépassant l'erreur maximale initiale
    var MatWorstCase : Array[Array[Int]] = copieMatrice(mat)
    var Select_Base : Int = 33554431 // 24 un de suite
    var maximum : Float = 0
    var MSSIMTEST : Float = 0

    //Afin de calculer l'espace maximal autorisee en fonction de l'erreur maximal, on calcul MSSIM entre les "bornes" (inferieur et superieur) et la matrice de base
    //Ainsi, on couvre l'ensemble des possiblites de valeurs des pixels : si les bornes respectent la limite alors quelconque information respectera la limite.
    for (espace <- 1 until 8){
      Select_Base = Select_Base - math.pow(2, espace).toInt - math.pow(2, espace+8).toInt - math.pow(2, espace + 16).toInt
      for (i <- 0 until mat.length){
        for (j <- 0 until mat(0).length){
          MatWorstCase(i)(j) = MatWorstCase(i)(j)&Select_Base
          MatWorstCase(i)(j) = MatWorstCase(i)(j) + (math.abs(((math.round(MatWorstCase(i)(j)/math.pow(2,espace)).toInt)%2-1)*math.pow(2,espace)).toInt +math.abs(((math.round(MatWorstCase(i)(j)/math.pow(2,espace+8)).toInt)%2-1)*math.pow(2,espace+8).toInt +math.abs(((math.round(MatWorstCase(i)(j)/math.pow(2,espace+16)).toInt)%2-1)*math.pow(2,espace+16).toInt)))
        }
      }
      MSSIMTEST = MSSIM(MatWorstCase,mat)
      maximum = 1-MSSIMTEST
      println(maximum)
      if (maximum > Erreur_Max){ //On compare l'erreur maximal autorisee et l'erreur trouvee
        println(espace-1)
        return (espace-1)
      }
    }
    return(8) //L'erreur n'est jamais atteinte (On peut changer tous les pixels (cas rare mais possible))
  }
  tailleMaxMessage(0.8,matBase)
  // Etape 2ter ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  //Cacher un message dans une image en fonction de l'espace disponible autorise
  def MessageCachee2(message:String,Image : ImageWrapper , n:Int){
    var mat : Array[Array[Int]]= Image.getImage()
    var BinaryText : String = TextToBinary(message) //On convertit le message en binaire
    var Select_Base : Int = 33554431 // 24 un de suite
    var indiceTexte : Int  = 0 //L'indice du bit a cacher texte en binaire
    // On calcul la valeur qui va permettre de liberer les places prises par le nouveau message (les remplacer par 0)
    for (j <- 1 until n) {
      Select_Base = Select_Base - math.pow(2, j-1).toInt - math.pow(2, j + 7).toInt - math.pow(2, j + 15).toInt
    }
    var nBinaire : String= n.toBinaryString
    for (j <- 0 until 4-nBinaire.length){ //Chaque caractere doit etre une combinaison de 8 bits (Par exemple , si on a un nombre binaire de 7 bits on ajoute un 0 au debut)
      nBinaire = "0" + nBinaire
    }
    for (i <- 1 until mat(0).length){
      mat(0)(i) = mat(0)(i)&Select_Base //On libere la place pour le message binaire
      for (j <- 1 to n){
        mat(0)(i) = BinaryText(indiceTexte%BinaryText.length)*math.pow(2, (j)-1).toInt + BinaryText((indiceTexte+1)%BinaryText.length)*math.pow(2, j+7).toInt + BinaryText((indiceTexte+2)%BinaryText.length)*math.pow(2, j+15).toInt
        indiceTexte = indiceTexte + 3
      }
    }
    for (i <- 1 until mat.length){
      for (j<- 0 until mat(0).length){
        mat(i)(j) = mat(i)(j)&Select_Base
        for (k <- 1 to n){
          mat(i)(j) = BinaryText(indiceTexte%BinaryText.length)*math.pow(2, (k)-1).toInt + BinaryText((indiceTexte+1)%BinaryText.length)*math.pow(2, k+7).toInt + BinaryText((indiceTexte+2)%BinaryText.length)*math.pow(2, k+15).toInt
          indiceTexte = indiceTexte + 3
        }
      }
    }
    Image.saveImage(outputFileEtape2ter)
  }

  // Fonctions auxiliaires Etapes 2bis et 2ter ---------------------------------------------------------------------------------------------------------------------------------------------------------------

  /*PAS TERMINEE


  //On veut retrouver le message dans l'image
  def MessageDecachee2(message:String,Image : ImageWrapper){
    //Entrée : Le message que l'on veut mettre dans l'image
    //         L'image que l'on veut utiliser
    //Sortie : /
    var mat : Array[Array[Int]]= Image.getImage()
    var BinaryText : String = TextToBinary(message) //On convertit le message en binaire
    var Select_Base : Int = 33554431 // 24 un de suite
    var indiceTexte : Int  = 0 //L'indice du bit a cacher texte en binaire
    var TexteBinaire : String = TextToBinary(message)
    // On calcul la valeur qui va permettre de liberer les places prises par le nouveau message (les remplacer par 0)
    for (j <- 1 until n) {
      Select_Base = Select_Base - math.pow(2, j-1).toInt - math.pow(2, j + 7).toInt - math.pow(2, j + 15).toInt
    }
    Select_Base = 33554431 -Select_Base // On prend le conjugue de Select_Base (les zeros deviennent des uns et inversement
    var nBinaire : String= n.toBinaryString
    for (j <- 0 until 4-nBinaire.length){ //Chaque caractere doit etre une combinaison de 8 bits (Par exemple , si on a un nombre binaire de 7 bits on ajoute un 0 au debut)
      nBinaire = "0" + nBinaire
    }
    for (i <- 1 until mat(0).length){
      mat(0)(i) = mat(0)(i)&Select_Base //On libere la place pour le message binaire
      for (j <- 1 to n){
        mat(0)(i) = BinaryText(indiceTexte%BinaryText.length)*math.pow(2, (j)-1).toInt + BinaryText((indiceTexte+1)%BinaryText.length)*math.pow(2, j+7).toInt + BinaryText((indiceTexte+2)%BinaryText.length)*math.pow(2, j+15).toInt
        indiceTexte = indiceTexte + 3
      }
    }
    for (i <- 1 until mat.length){
      for (j<- 0 until mat(0).length){
        for (k <- 1 to n){
          mat(i)(j) = BinaryText(indiceTexte%BinaryText.length)*math.pow(2, (k)-1).toInt + BinaryText((indiceTexte+1)%BinaryText.length)*math.pow(2, k+7).toInt + BinaryText((indiceTexte+2)%BinaryText.length)*math.pow(2, k+15).toInt
          indiceTexte = indiceTexte + 3
        }
      }
    }
  }
  */


  //Conversion du texte en Binaire
  def TextToBinary(message : String):String={
    //Entrée : Le message sous forme String
    //Sortie : Le message codé en binaire grace au tableau ASCII
    var TexteASCII = message.getBytes
    var SingleBinaryText : String = ""
    var TextBinary : String = ""
    for (i <- 0 to TexteASCII.length -1 ){
      SingleBinaryText = TexteASCII(i).toBinaryString //On convertit le nombre decimal en nombre binaire
      for (j <- 0 until 8-(TexteASCII(i).toBinaryString).length){ //Chaque caractere doit etre une combinaison de 8 bits (Par exemple , si on a un nombre binaire de 7 bits on ajoute un 0 au debut)
        SingleBinaryText = "0" + SingleBinaryText
      }
      TextBinary += (SingleBinaryText) //On ajoute l'octet au texte binaire
    }
    return(TextBinary)
  }

  //Conversion du texte en Binaire
  def BinaryToText(message : String):String= {
    //Entrée : Le message codé en binaire
    //Sortie : Le message sous forme String grace au tableau ASCII
    var matTransfo: Array[Int] = new Array[Int](message.length / 8)
    var Text: String = ""
    for (i <- 0 until (message.length) / 8) {
      matTransfo(i) = Integer.parseInt(message.slice(8 * i, 8 * (i + 1)), 2)
      Text = matTransfo.map(_.toChar).mkString
    }
    return (Text)
  }

  def printMat(mat:Array[Array[Int]]){
    for (i <- 0 until mat.length){
      for (j <- 0 until mat(0).length){
        println(mat(i)(j).toBinaryString)
      }
    }
  }

  def copieMatrice(mat:Array[Array[Int]]):Array[Array[Int]]={
    var NewMat : Array[Array[Int]]=Array.ofDim(mat.length,mat(0).length)
    for (i <- 0 until mat.length){
      for (j <- 0 until mat(0).length){
        NewMat(i)(j) = mat(i)(j)
      }
    }
    return(NewMat)
  }



  //Etape 3 : Nouvelle méthode ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  //Caractere ASCII DEC(HEX) : 32(20) -> 255 (FF)
  def Cryptage_1 (message :String):String={
    //Entree : Le message a crypter
    //Sortie : Le message crypte par la methode de Cesar
    var New_Message : String = ""
    var message_bytes = message.getBytes
    var Valeur_Decalage = message.length % 255
    for (i <- 0 until message.length){
      if (message_bytes(i)+Valeur_Decalage > 255){
        message_bytes(i)=(message_bytes(i).toInt + Valeur_Decalage - (255-32)).toByte
      }
      else {
        message_bytes(i) = (message_bytes(i) + Valeur_Decalage).toByte
      }
    }
    New_Message = (message_bytes.map(_.toChar)).mkString
    println(New_Message)
    return (New_Message)
  }

  def Decryptage_1 (message :String):String={
    //Entree : Le message crypte
    //Sortie : Le message decrypte
    var New_Message : String = ""
    var message_bytes = message.getBytes
    var Valeur_Decalage = message.length % 255
    for (i <- 0 until message.length){
      if (message_bytes(i)-Valeur_Decalage < 32 ){
        message_bytes(i)=(message_bytes(i).toInt - Valeur_Decalage + (255-32)).toByte
      }
      else {
        message_bytes(i) = (message_bytes(i) - Valeur_Decalage).toByte
      }
    }
    New_Message = (message_bytes.map(_.toChar)).mkString
    println(New_Message)
    return (New_Message)
  }


  def test () {
    //l'utilisateur choisit quelle étape il souhaite tester
    println("Quelle étape voulez-vous tester ? (réponses acceptées : -1, 0, 1, 2, 2 bis, 2 ter, 3)")
    val input = scala.io.StdIn.readLine()

    //étape -1
    if (input == "-1") {
      println("Pour ce test, nous vous proposons d'appliquer notre fonction sur deux images que vous aurez choisies parmi une banque d'images. Vous pourrez ainsi remarquer à l'oeil nu si l'image est bien cachée ou non.")
      println("Vous avez le choix entre : Douceurs.png, Galettes.png, Homme.jpeg, Jeu.png, Login.jpeg, Courbe.png")

      println("Quelle image souhaitez-vous cacher ? (Veuillez entrez le nom complet de l'image, exemple : Jeu.png)")
      val input1 = scala.io.StdIn.readLine()

      if (input1 != "Douceurs.png" && input1 != "Galettes.png" && input1 != "Homme.jpeg" && input1 != "Jeu.png" && input1 != "Login.jpeg" && input1 != "Courbe.png") {
        println("Cette réponse n'est pas acceptée, veuillez recommencer")
        test()
      }
      else {
        var Image1: String = "Image/" + input1
        var wrapped1: ImageWrapper = new ImageWrapper(Image1);

        println("Dans quelle image souhaitez-vous la cacher ? (Veuillez entrez le nom complet de l'image, exemple : Jeu.png)")
        val input2 = scala.io.StdIn.readLine()

        if (input2 != "Douceurs.png" && input2 != "Galettes.png" && input2 != "Homme.jpeg" && input2 != "Jeu.png" && input2 != "Login.jpeg" && input2 != "Courbe.png") {
          println("Cette réponse n'est pas acceptée, veuillez recommencer")
          test()
        }
        else {
          var Image2: String = "Image/" + input2
          var wrapped2: ImageWrapper = new ImageWrapper(Image2);

          cachee(wrapped2, wrapped1)
        }
      }
    }

    //étape 0
    else if (input == "0") {
      println("Pour ce test, nous vous proposons d'appliquer notre fonction sur une image qui possède une image cachée que vous aurez choisi parmi une banque d'images. Vous pourrez ainsi remarquer à l'oeil nu si l'image est bien décodée ou non.")
      println("Vous avez le choix entre : CourbeJeu.png, CourbeGalettes.png, GalettesDouceurs.png, GalettesCourbe.png, LoginHomme.png, LoginJeu.png")

      println("Quelle image souhaitez-vous décoder ? (Veuillez entrez le nom complet de l'image, exemple : LoginJeu.png)")
      val input1 = scala.io.StdIn.readLine()

      if (input1 != "CourbeJeu.png" && input1 != "CourbeGalettes" && input1 != "GalettesDouceurs" && input1 != "GalettesCourbe.png" && input1 != "LoginHomme" && input1 != "LoginJeu.png") {
        println("Cette réponse n'est pas acceptée, veuillez recommencer")
        test()
      }
      else {
        var Image1: String = "Image/" + input1
        var wrapped1: ImageWrapper = new ImageWrapper(Image1);

        retrouve(wrapped1)
      }
    }

    //étape 1
    else if (input == "1") {
      //choix de l'image à décoder et de l'image à laquelle on veut la comparer
      println("Pour ce test, nous vous proposons d'appliquer notre fonction sur deux images afin de voir la perte de qualité entre l'image décodée et l'image d'origine")
      println("Choisissez l'image que vous voulez décoder parmi :  CourbeJeu.png, CourbeGalettes.png, GalettesDouceurs.png, GalettesCourbe.png, LoginHomme.png, LoginJeu.png (Veuillez entrez le nom complet de l'image, exemple : LoginJeu.png)")
      val dec = scala.io.StdIn.readLine();
      val Adecode: String = "Image/" + dec;
      val ImgAdecode: ImageWrapper = new ImageWrapper(Adecode);

      println("Choisissez l'image à laquelle vous voulez la comparer parmi : Douceurs.png, Galettes.png, Homme.jpeg, Jeu.png, Login.jpeg, Courbe.png (Veuillez entrez le nom complet de l'image, exemple : LoginJeu.png)")
      val comp = scala.io.StdIn.readLine();
      val Acomp: String = "Image/" + comp;
      val ImgAcomp: ImageWrapper = new ImageWrapper(Acomp);

      retrouve(ImgAdecode)
      val DECO : ImageWrapper = new ImageWrapper("Image/ImageDecoder.png");

      //test de taille des images sélectionnées
      if (!testTaille(DECO.getImage(),ImgAcomp.getImage())){
        println("Images de même taille requises, veuillez recommencer")
        test()
      }

      //tests d'existence des images demandées par l'utilisateur
      else if (dec != "CourbeJeu.png" && dec != "CourbeGalettes.png" && dec != "GalettesDouceurs.png" && dec != "GalettesCourbe.png" && dec != "LoginHomme.png" && dec != "LoginJeu.png") {
        println("Cette réponse n'est pas acceptée, veuillez recommencer")
        test()
      }
      else if (comp != "Douceurs.png" && comp != "Galettes.png" && comp != "Homme.jpeg" && comp != "Jeu.png" && comp != "Login.jpeg" && comp != "Courbe.png") {
        println("Cette réponse n'est pas acceptée, veuillez recommencer")
        test()
      }

      //cas où tous les tests préalables ont été passés
      else {
        //choix de la méthode à tester
        println("Veuillez choisir la méthode à tester parmi : EQM, SSIM")
        val methode = scala.io.StdIn.readLine();

        //cas où on tape autre chose que les deux méthodes
        if (methode != "EQM" && methode != "SSIM"){
          println("Cette réponse n'est pas acceptée, veuillez recommencer")
          test()
        }
        //cas favorable
        else {
          //méthode SSIM
          if (methode == "SSIM") {
            //cette partie ne renvoie plus d'erreur, mais cela ne renvoie rien malgré que la fonction MSSIM est censée renvoyer quelque chose
            MSSIM(ImgAcomp.getImage(), DECO.getImage())
          }
          //méthode EQM
          else if (methode == "EQM"){
              EQM(ImgAcomp,DECO) //cette partie fonctionne
            }
        }
      }

    }

    //étape 2
    else if (input == "2") {
      println("Pour ce test, nous vous proposons d'appliquer notre fonction sur deux images que vous aurez choisies parmi une banque d'images. Vous pourrez ainsi remarquer à l'oeil nu si l'image est bien cachée ou non.")
      //choix de l'image que l'on veut cacher
      println("Vous avez le choix entre : Douceurs.png, Galettes.png, Homme.jpeg, Jeu.png, Login.jpeg, Courbe.png")
      println("Quelle image souhaitez-vous cacher ? (Veuillez entrez le nom complet de l'image, exemple : Jeu.png)")
      val input1 = scala.io.StdIn.readLine()

      //cas où le nom de l'image demandée ne correspond à aucune image du dossier
      if (input1 != "Douceurs.png" && input1 != "Galettes.png" && input1 != "Homme.jpeg" && input1 != "Jeu.png" && input1 != "Login.jpeg" && input1 != "Courbe.png") {
        println("Cette réponse n'est pas acceptée, veuillez recommencer")
        test()
      }
      //cas favorable
      else {
        var Image1: String = "Image/" + input1
        var wrapped1: ImageWrapper = new ImageWrapper(Image1);

        //choix de l'image support
        println("Dans quelle image souhaitez-vous la cacher ? (Veuillez entrez le nom complet de l'image, exemple : Jeu.png)")
        val input2 = scala.io.StdIn.readLine()

        //cas où le nom de l'image demandée ne correspond à aucune image du dossier
        if (input2 != "Douceurs.png" && input2 != "Galettes.png" && input2 != "Homme.jpeg" && input2 != "Jeu.png" && input2 != "Login.jpeg" && input2 != "Courbe.png") {
          println("Cette réponse n'est pas acceptée, veuillez recommencer")
          test()
        }

        //cas favorable
        else {
          var Image2: String = "Image/" + input2
          var wrapped2: ImageWrapper = new ImageWrapper(Image2);

          println("Combien de bits transparents souhaitez-bous modifier ?")
          val T = scala.io.StdIn.readLine()

          println("Combien de bits rouges souhaitez-bous modifier ?")
          val R = scala.io.StdIn.readLine()

          println("Combien de bits verts souhaitez-bous modifier ?")
          val V = scala.io.StdIn.readLine()

          println("Combien de bits bleus souhaitez-bous modifier ?")
          val B = scala.io.StdIn.readLine()

          etape2(wrapped2, wrapped1,T.toInt,R.toInt,V.toInt,B.toInt)
        }
      }
    }

    //étape 2bis
    else if (input == "2 bis") {

    }

    //étape 2ter
    else if (input == "2 ter") {

    }

    //étape 3
    else if (input == "3") {
      println("Souhaitez-vous crypter ou décrupter un message ? (Répondre par les mots crypter ou decrypter")
      val choice = scala.io.StdIn.readLine();

      if (choice != "crypter" && choice != "decrypter"){
        println("Cette réponse n'est pas acceptée, veuillez recommencer")
        test()

      }

      else {
        if (choice == "crypter") {
          println("Tapez le message à crypter :")
          val messACrypter = scala.io.StdIn.readLine();
          Cryptage_1(messACrypter)
        }

        if (choice == "decrypter") {
          println("Tapez le message crypté :")
          val messADecrypter = scala.io.StdIn.readLine();
          Decryptage_1(messADecrypter)
        }
      }

    }

    //cas où aucune des entrées ne convient
    else {
      println("Cette réponse n'est pas acceptée, veuillez réessayer.")
      test
    }
  }

  def testTaille(img1:Array[Array[Int]],img2:Array[Array[Int]]):Boolean={
    //entrée : matrices correspondant aux 2 images dont on veut comparer la taille
    //sortie : booléen qui dit si les images sont ou non de même taille
    //on compare directement la largeur et la hauteur des images
    val LONG1 : Int = img1.length;
    val LARG1 : Int = img1(0).length;
    val LONG2 : Int = img2.length;
    val LARG2 : Int = img2(0).length;
    var ResTest : Boolean = false;
    if (LONG1 == LONG2 && LARG1 == LARG2){
      ResTest = true;
    }
    return(ResTest)
  }

  //test
  //retrouve(wrappedImageCoder)
  val a : String = "Image/Jeu.png"
  val b : ImageWrapper = new ImageWrapper(a)
  val c : Array[Array[Int]] = b.getImage()
  val d : String = "Image/Douceurs.png"
  val e : ImageWrapper = new ImageWrapper(d)
  val f : Array[Array[Int]] = e.getImage()
  val g : String = "Image/Homme.jpeg"
  val h : ImageWrapper = new ImageWrapper(g)
  val k : Array[Array[Int]] = h.getImage()

  //MSSIM(c,f)
  //MSSIM(c,c)
  //EQM(b,e)
  //EQM(b,b)

}

