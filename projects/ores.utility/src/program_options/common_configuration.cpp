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
#include "ores.utility/program_options/common_configuration.hpp"

namespace ores::utility::program_options {

namespace {

const std::string help_arg("help");
const std::string version_arg("version");
const std::string verbose_arg("verbose");

}

boost::program_options::options_description
common_configuration::make_options_description() {
    using boost::program_options::options_description;

    options_description r("General");
    r.add_options()
        ("help,h", "Display usage and exit.")
        ("version,v", "Output version information and exit.")
        ("verbose", "Enable verbose output.");

    return r;
}

common_options common_configuration::
read_options(const boost::program_options::variables_map& vm) {
    common_options r;
    r.verbose = vm.count(verbose_arg) != 0;
    return r;
}

}
