/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.utility/program_options/shared_domain_registry.hpp"

namespace ores::utility::program_options {

namespace {

std::map<std::string, std::set<std::string>>& mutable_domains() {
    static std::map<std::string, std::set<std::string>> r;
    return r;
}

}

void shared_domain_registry::register_domain(const std::string& prefix,
                                              const std::set<std::string>& allowed_suffixes) {
    mutable_domains()[prefix].insert(allowed_suffixes.begin(), allowed_suffixes.end());
}

const std::map<std::string, std::set<std::string>>& shared_domain_registry::domains() {
    return mutable_domains();
}

}
