dir1 = getDirectory("Choose Source Directory ");
dir2 = getDirectory("Choose Destination Directory ");
dir3 = getDirectory("Choose Destination Directory ");
list = getFileList(dir1);

setBatchMode(true);

for (i=0; i<list.length; i++) {

    showProgress(i+1, list.length);

    filename = dir1 + list[i];

    if (endsWith(filename, "JPG")) {
        open(filename);

        run("Apply saved SIOX segmentator", "browse=[/Users/mfeldmann/Desktop/Reading/Strawberry_Research/Strawberry imaging/basic_img_scripts/segmentator-NormalMulti.siox] siox=[/Users/mfeldmann/Desktop/Reading/Strawberry_Research/Strawberry imaging/basic_img_scripts//segmentator-NormalMulti.siox]");

        saveAs("JPEG", dir2+"mN_"+list[i]);

        run("Close");

close();
}
}

for (i=0; i<list.length; i++) {

    showProgress(i+1, list.length);

    filename = dir1 + list[i];

    if (endsWith(filename, "JPG")) {
        open(filename);

        run("Apply saved SIOX segmentator", "browse=[/Users/mfeldmann/Desktop/Reading/Strawberry_Research/Strawberry imaging/basic_img_scripts/segmentator-NormalMulti.siox] siox=[/Users/mfeldmann/Desktop/Reading/Strawberry_Research/Strawberry imaging/basic_img_scripts//segmentator-LightMulti.siox]");

        saveAs("JPEG", dir3+"mL_"+list[i]);

        run("Close");

close();
}
}

setBatchMode(false);
