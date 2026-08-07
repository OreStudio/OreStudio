/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.platform/environment/environment.hpp"
#include "ores.platform/environment/real_environment_provider.hpp"
#include <stdexcept>
#include <string>

namespace ores::platform::environment {

namespace {
    thread_local environment_provider* current_provider = nullptr;
}

environment_provider& environment::default_provider_instance() {
    static real_environment_provider instance;
    return instance;
}

environment_provider& environment::provider() {
    if (current_provider == nullptr)
        return default_provider_instance();
    return *current_provider;
}

environment_provider* environment::swap_provider(environment_provider* new_provider) {
    auto* old = current_provider;
    current_provider = new_provider;
    return old;
}

std::optional<std::string> environment::get_value(const std::string& name) {
    return provider().get(name);
}

std::string environment::get_value_or_default(const std::string& name,
                                              const std::string& default_value) {
    auto value = get_value(name);
    return value.value_or(default_value);
}

int environment::get_int_value_or_default(const std::string& name, int default_value) {
    auto value = get_value(name);
    if (!value.has_value())
        return default_value;

    try {
        return std::stoi(value.value());
    } catch (...) {
        return default_value;
    }
}

std::string environment::get_value_or_throw(const std::string& name) {
    auto value = get_value(name);
    if (!value.has_value())
        throw std::runtime_error("Required environment variable not set: " + name);
    return *value;
}

void environment::set_value(const std::string& name, const std::string& value) {
    provider().set(name, value);
}

void environment::unset_value(const std::string& name) {
    provider().unset(name);
}

}
