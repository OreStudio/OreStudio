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
#include "ores.comms/config/compression_configuration.hpp"

#include "ores.comms/messaging/handshake_protocol.hpp"

namespace ores::comms::config {

namespace {

const std::string compression_enabled_arg("compression-enabled");
const std::string compression_algorithm_arg("compression-algorithm");

}

boost::program_options::options_description
compression_configuration::make_options_description() {
    using boost::program_options::value;
    using boost::program_options::options_description;

    options_description r("Compression");
    r.add_options()
        ("compression-enabled,c",
            "Enable compression for network communication.")
        ("compression-algorithm", value<std::string>()->default_value("all"),
            "Compression algorithm: zlib, gzip, bzip2, all.");

    return r;
}

compression_options compression_configuration::
read_options(const boost::program_options::variables_map& vm) {
    compression_options r;

    r.enabled = vm.count(compression_enabled_arg) != 0;

    if (vm.count(compression_algorithm_arg) != 0) {
        r.algorithm = vm[compression_algorithm_arg].as<std::string>();
    }

    return r;
}

std::uint8_t compression_configuration::
to_compression_mask(const compression_options& opts) {
    using namespace messaging;

    if (!opts.enabled) {
        return 0;
    }

    if (opts.algorithm == "zlib") {
        return COMPRESSION_SUPPORT_ZLIB;
    } else if (opts.algorithm == "gzip") {
        return COMPRESSION_SUPPORT_GZIP;
    } else if (opts.algorithm == "bzip2") {
        return COMPRESSION_SUPPORT_BZIP2;
    } else {
        // "all" or any unrecognized value defaults to all
        return COMPRESSION_SUPPORT_ALL;
    }
}

}
