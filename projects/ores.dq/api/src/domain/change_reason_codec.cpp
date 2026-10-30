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
 * Template: cpp_domain_type_codec.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.dq.api/domain/change_reason_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of change_reason, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto change_reason_codec_fields = std::tuple{
    utility::codec::field{"version", &change_reason::version},
    utility::codec::field{"tenant_id", &change_reason::tenant_id},
    utility::codec::field{"code", &change_reason::code},
    utility::codec::field{"description", &change_reason::description},
    utility::codec::field{"category_code", &change_reason::category_code},
    utility::codec::field{"applies_to_new", &change_reason::applies_to_new},
    utility::codec::field{"applies_to_amend", &change_reason::applies_to_amend},
    utility::codec::field{"applies_to_delete", &change_reason::applies_to_delete},
    utility::codec::field{"requires_commentary", &change_reason::requires_commentary},
    utility::codec::field{"display_order", &change_reason::display_order},
    utility::codec::field{"modified_by", &change_reason::modified_by},
    utility::codec::field{"performed_by", &change_reason::performed_by},
    utility::codec::field{"change_reason_code", &change_reason::change_reason_code},
    utility::codec::field{"change_commentary", &change_reason::change_commentary},
    utility::codec::field{"recorded_at", &change_reason::recorded_at},
};

}

std::string change_reason_codec::to_json(const change_reason& v) {
    return utility::codec::object::write_json(v, change_reason_codec_fields);
}

change_reason change_reason_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<change_reason>(text, change_reason_codec_fields);
}

std::string change_reason_codec::to_msgpack(const change_reason& v) {
    return utility::codec::object::write_msgpack(v, change_reason_codec_fields);
}

change_reason change_reason_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<change_reason>(data, change_reason_codec_fields);
}

}
