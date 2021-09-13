<?php
declare(strict_types=1);

namespace App\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * @ORM\Entity
 */
class AuthToken
{
    private $token;

    private $extra;

    /**
     * @var App\\Entity\\User
     */
    private $user;

    /**
     * @var bool
     */
    private $valid = false;

    /**
     * @var \DateTime
     */
    private $creation_time;

    public function __construct(
        string $token,
        User $user,
        bool $valid = false,
        ?\DateTime $creation_time = null
    ) {
        $this->token = $token;
        $this->user = $user;
        $this->valid = $valid;
        $this->creation_time = $creation_time ?? new \DateTime();
    }

    public function getToken(): bool
    {
        return $this->token;
    }

    public function getUser(): User
    {
        return $this->user;
    }

    public function hasStudentRole(): bool
    {
        return $this->role_student;
    }

    public function isValid(): bool
    {
        return $this->valid;
    }

    public function anAddedFunction()
    {
        return $this->extra;
    }

    public function getCreationTime(): \DateTime
    {
        return $this->creation_time;
    }
}
