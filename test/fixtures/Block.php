{
    return new Response(
        $this->twig->render('domain/manage.html.twig', [
            'domain' => $this->repo->find($name),
            'users' => $this->user_repo->findBy(['domain' => $name])
        ])
    );
}
