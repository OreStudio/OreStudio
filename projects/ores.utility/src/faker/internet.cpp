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
#include "ores.utility/faker/internet.hpp"
#include <boost/asio/ip/address.hpp>

#include <faker-cxx/faker.h> // IWYU pragma: keep.

namespace ores::utility::faker {

std::string internet::endpoint() {
    const std::string ip = std::string(::faker::internet::ipv4());
    const int port = ::faker::number::integer(1025, 65535);
    return ip + ":" + std::to_string(port);
}

boost::asio::ip::address internet::ipv4() {
    return boost::asio::ip::make_address(::faker::internet::ipv4());
}

}
