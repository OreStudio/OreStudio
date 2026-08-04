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
#include "ores.utility/program_options/environment_mapper_factory.hpp"
#include "ores.utility/program_options/shared_domain_registry.hpp"
#include <algorithm>
#include <ranges>

namespace ores::utility::program_options {

namespace {

std::string strip_prefix_to_option_name(const std::string& env_var, const std::string& prefix) {
    auto env_body = env_var | std::views::drop(prefix.size());
    std::string option_name;

    std::ranges::transform(env_body, std::back_inserter(option_name), [](unsigned char c) -> char {
        if (c == '_')
            return '-';
        return std::tolower(c);
    });

    return option_name;
}

}

std::function<std::string(const std::string&)>
environment_mapper_factory::make_mapper(const std::string app_name) {
    return [app_name](const std::string& env_var) -> std::string {
        const std::string app_prefix = "ORES_" + app_name + "_";
        if (env_var.starts_with(app_prefix))
            return strip_prefix_to_option_name(env_var, app_prefix);

        for (const auto& domain : shared_domain_registry::domains()) {
            const std::string domain_prefix = "ORES_" + domain + "_";
            if (env_var.starts_with(domain_prefix))
                return strip_prefix_to_option_name(env_var, domain_prefix);
        }

        return {};
    };
}

}
