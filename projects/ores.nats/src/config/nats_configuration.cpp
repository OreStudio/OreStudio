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
#include "ores.nats/config/nats_configuration.hpp"

namespace ores::nats::config {

namespace {

const std::string nats_url_arg("nats-url");
const std::string nats_subject_arg("nats-subject");

}

boost::program_options::options_description
nats_configuration::make_options_description(
    const std::string& default_subject) {
    using boost::program_options::value;
    using boost::program_options::options_description;

    options_description r("NATS");
    r.add_options()
        (nats_url_arg.c_str(),
            value<std::string>()->default_value("nats://localhost:4222"),
            "NATS server URL.")
        (nats_subject_arg.c_str(),
            value<std::string>()->default_value(default_subject),
            "NATS subject on which the service listens for requests.");

    return r;
}

nats_options nats_configuration::read_options(
    const boost::program_options::variables_map& vm) {
    nats_options r;
    r.url = vm[nats_url_arg].as<std::string>();
    r.subject = vm[nats_subject_arg].as<std::string>();
    return r;
}

}
