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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_table.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.inbox.api/domain/notification_recipient_table.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>
#include <sstream>

namespace ores::inbox::domain {

namespace {
template <typename T>
std::string opt_str(const std::optional<T>& o) {
    if (!o)
        return {};
    std::ostringstream s;
    if constexpr (std::is_same_v<T, bool>)
        s << std::boolalpha;
    s << *o;
    return s.str();
}
}

std::string convert_to_table(const std::vector<notification_recipient>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Notification" << "Account" << "Read At" << "Cleared At" << "Version"
          << fort::endr;

    for ([[maybe_unused]] const auto& nr : v) {
        table << boost::uuids::to_string(nr.notification_id)
              << boost::uuids::to_string(nr.account_id) << opt_str(nr.read_at)
              << opt_str(nr.cleared_at) << nr.version << fort::endr;
    }
    return table.to_string();
}

}
