<?php
namespace App\Functions;

function MergeTwoArraysAndSomeOtherStuff(array $array1, $untyped_variable): Response
{
    $merged = array_merge($array_1, $untyped_variable);

    $mapped = arrap_map(
        function ($item) {
            return $item;
        },
        $merged
    );

    $user = $this->user_repo->findOne($req->get('user'));

    return new Response(
        $this->twig->render('address/create.html.twig', [
            'user' => $user,
        ])
    );
}


function BeTheSecondFunctionInTheFile() {
    return [ "Very Impressive Result" => $result ];
}
