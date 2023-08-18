<?php
declare(strict_types=1);

function generateClassStub(\ReflectionClass $class): string
{
    $stub = '';

    $hasNamespace = '' !== $class->getNamespaceName();
    if ($hasNamespace) {
        $stub = 'namespace ' . $class->getNamespaceName() . ' {' . PHP_EOL;
    }

    if ($class->isFinal()) {
        $stub .= 'final ';
    }

    if ($class->isAbstract() && !$class->isInterface()) {
        $stub .= 'abstract ';
    }

    if ($class->isInterface()) {
        $stub .= 'interface ';
    } else if ($class->isTrait()) {
        $stub .= 'trait ';
    } else {
        $stub .= 'class ';
    }

    $stub .= $class->getShortName() . ' {' . PHP_EOL;

    foreach ($class->getMethods() as $method) {
        if ($method->isFinal()) {
            $stub .= 'final ';
        }

        if ($method->isPublic()) {
            $stub .=  'public ';
        } else if ($method->isPrivate()) {
            $stub .= 'private ';
        } else if ($method->isProtected()) {
            $stub .= 'protected ';
        }

        if ($method->isStatic()) {
            $stub .= 'static ';
        }

        $stub .= generateFunctionStub($method);
    }

    $stub .= '}' . PHP_EOL;

    if ($hasNamespace) {
        $stub .= '}' . PHP_EOL; // Close namespace block
    }

    return $stub;
}

function generateFunctionStub(\ReflectionFunctionAbstract $function): string
{
    $stub = 'function ' . $function->getName() . '(';

    $parameters = [];
    foreach ($function->getParameters() as $ref_parameter) {
        $parameter = '';

        if ($ref_parameter->hasType()) {
            $parameter .= $ref_parameter->getType()->__toString() . ' ';
        }

        $parameter .= '$' . $ref_parameter->getName();

        if ($ref_parameter->isDefaultValueAvailable()) {
            $parameter .= ' = ' . var_export($ref_parameter->getDefaultValue(), true);
        }

        $parameters[] = $parameter;
    }
    $stub .= implode(', ', $parameters);

    $stub .= ')';

    if ($function->hasReturnType()) {
        $stub .= ': ' . $function->getReturnType()->__toString();
    } else if ($function->hasTentativeReturnType()) {
        $stub .= ': ?' . $function->getTentativeReturnType();
    }

    $stub .= ' {}' . PHP_EOL;

    return $stub;
}

foreach (get_defined_functions(false)['internal'] as $function_name) {
    echo generateFunctionStub(new ReflectionFunction($function_name));
}

foreach ([...get_declared_classes(), ...get_declared_interfaces(), ...get_declared_traits()] as $class_name) {
    echo generateClassStub(new ReflectionClass($class_name));
}
