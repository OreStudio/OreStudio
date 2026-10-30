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
#include "ores.dq.api/domain/badge_definition_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of badge_definition, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto badge_definition_codec_fields = std::tuple{
    utility::codec::field{"version", &badge_definition::version},
    utility::codec::field{"tenant_id", &badge_definition::tenant_id},
    utility::codec::field{"code", &badge_definition::code},
    utility::codec::field{"name", &badge_definition::name},
    utility::codec::field{"description", &badge_definition::description},
    utility::codec::field{"background_colour", &badge_definition::background_colour},
    utility::codec::field{"text_colour", &badge_definition::text_colour},
    utility::codec::field{"severity_code", &badge_definition::severity_code},
    utility::codec::field{"css_class", &badge_definition::css_class},
    utility::codec::field{"display_order", &badge_definition::display_order},
    utility::codec::field{"modified_by", &badge_definition::modified_by},
    utility::codec::field{"performed_by", &badge_definition::performed_by},
    utility::codec::field{"change_reason_code", &badge_definition::change_reason_code},
    utility::codec::field{"change_commentary", &badge_definition::change_commentary},
    utility::codec::field{"recorded_at", &badge_definition::recorded_at},
};

}

std::string badge_definition_codec::to_json(const badge_definition& v) {
    return utility::codec::object::write_json(v, badge_definition_codec_fields);
}

badge_definition badge_definition_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<badge_definition>(text, badge_definition_codec_fields);
}

std::string badge_definition_codec::to_msgpack(const badge_definition& v) {
    return utility::codec::object::write_msgpack(v, badge_definition_codec_fields);
}

badge_definition badge_definition_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<badge_definition>(data,
                                                                  badge_definition_codec_fields);
}

}
