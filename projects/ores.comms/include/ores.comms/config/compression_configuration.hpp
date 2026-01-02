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
#ifndef ORES_COMMS_CONFIG_COMPRESSION_CONFIGURATION_HPP
#define ORES_COMMS_CONFIG_COMPRESSION_CONFIGURATION_HPP

#include <string>
#include <cstdint>
#include <boost/program_options.hpp>

namespace ores::comms::config {

/**
 * @brief Compression options parsed from command line.
 */
struct compression_options {
    bool enabled = false;
    std::string algorithm = "all";
};

/**
 * @brief Provides command-line options for compression configuration.
 *
 * Standard compression options:
 *   -c, --compression-enabled     Enable compression for network communication
 *       --compression-algorithm   Algorithm: zlib, gzip, bzip2, all (default: all)
 */
class compression_configuration final {
public:
    compression_configuration() = delete;

    /**
     * @brief Creates the options description for compression CLI arguments.
     *
     * @return options_description for compression configuration.
     */
    static boost::program_options::options_description
    make_options_description();

    /**
     * @brief Reads compression options from parsed variables map.
     *
     * @param vm Parsed command-line options.
     * @return compression_options populated from the variables map.
     */
    static compression_options
    read_options(const boost::program_options::variables_map& vm);

    /**
     * @brief Converts compression options to protocol bitmask.
     *
     * @param opts The compression options.
     * @return Bitmask suitable for use in client_options.supported_compression.
     */
    static std::uint8_t to_compression_mask(const compression_options& opts);
};

}

#endif
