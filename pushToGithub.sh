echo -e "Pushing build to github-pages"

cp dist/canvashs*.tar.gz $HOME/canvashs.tar.gz

cd $HOME
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Travis"

git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com:CanvasHS/Canvas.hs.git gh-pages > /dev/null

cd gh-pages/dists
cp $HOME/canvashs*.tar.gz canvashs-${TRAVIS_BUILD_NUMBER}.tar

git add .
git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to gh-pages"
git push -fq origin gh-pages > /dev/null

echo -e "build pushed to gh-pages"
