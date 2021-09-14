<?php
declare(strict_types=1);

namespace App\Controller;

use Symfony\Component\HttpFoundation\Response;
use App\Entity\Address;
use Symfony\Component\HttpFoundation\RedirectResponse;
use App\Repository\AddressRepository;
use App\Repository\UserRepository;
use Doctrine\ORM\EntityManagerInterface;
use Twig\Environment;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\Routing\Annotation\Route;

class AddressController
{
    const A_CONSTANT_FOR_THE_SAKE_OF_HAVING_ONE = 'a value';
    public const ARRAY_CONSTANT = [
        'key' => 'value',
        'key' => 0
    ];

    private $repo;
    private $user_repo;
    private $twig;
    private $em;

    public function __construct(
        AddressRepository $repo,
        UserRepository $user_repo,
        Environment $twig,
        EntityManagerInterface $em
    ) {
        $this->repo = $repo;
        $this->user_repo = $user_repo;
        $this->twig = $twig;
        $this->em = $em;
    }

    /**
     * @Route("/address/add", methods={"GET"})
     */
    public function addAddressPage(Request $req): Response
    {
        $user = $this->user_repo->findOne($req->get('user'));

        return new Response(
            $this->twig->render('address/create.html.twig', [
                'user' => $user,
            ])
        );
    }

    /**
     * @Route("/address/add", methods={"POST"})
     */
    public function addAddressAction(Request $req): Response
    {
        $user = $this->user_repo->findOne($req->request->get('user'));
        $address_string = $req->request->get('address');

        $address = new Address($user, $address_string);

        $this->em->persist($address);
        $this->em->flush();


        return new RedirectResponse('/user/' . $user->getLoginName() . '/manage');
    }

    /**
     * @Route("/address/delete", methods={"POST"})
     */
    public function deleteAddressAction(Request $req): Response
    {
        $address = $this->repo->find($req->request->get('address'));

        // This is what a while looks like to phpinspect when it parses up
        // until "point" to complete.
        $this->em->remove($this->em->
