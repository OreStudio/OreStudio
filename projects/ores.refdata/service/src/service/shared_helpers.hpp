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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_REFDATA_SERVICE_SRC_SERVICE_SHARED_HELPERS_HPP_
#define ORES_REFDATA_SERVICE_SRC_SERVICE_SHARED_HELPERS_HPP_

#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::service {

inline std::string text_of(const boost::uuids::uuid& v) {
    return boost::uuids::to_string(v);
}

inline std::string text_of(const std::optional<boost::uuids::uuid>& v) {
    return v ? boost::uuids::to_string(*v) : std::string{};
}

inline std::string text_of(const std::optional<std::string>& v) {
    return v ? *v : std::string{};
}

inline std::string text_of(bool v) {
    return v ? "true" : "false";
}

inline std::string text_of(const std::string& v) {
    return v;
}

}

#endif
