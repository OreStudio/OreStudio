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
#include "ores.dq.api/domain/fsm_state_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of fsm_state, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto fsm_state_codec_fields = std::tuple{
    utility::codec::field{"version", &fsm_state::version},
    utility::codec::field{"tenant_id", &fsm_state::tenant_id},
    utility::codec::field{"id", &fsm_state::id},
    utility::codec::field{"machine_id", &fsm_state::machine_id},
    utility::codec::field{"name", &fsm_state::name},
    utility::codec::field{"is_initial", &fsm_state::is_initial},
    utility::codec::field{"is_terminal", &fsm_state::is_terminal},
    utility::codec::field{"modified_by", &fsm_state::modified_by},
    utility::codec::field{"performed_by", &fsm_state::performed_by},
    utility::codec::field{"change_reason_code", &fsm_state::change_reason_code},
    utility::codec::field{"change_commentary", &fsm_state::change_commentary},
    utility::codec::field{"recorded_at", &fsm_state::recorded_at},
};

}

std::string fsm_state_codec::to_json(const fsm_state& v) {
    return utility::codec::object::write_json(v, fsm_state_codec_fields);
}

fsm_state fsm_state_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<fsm_state>(text, fsm_state_codec_fields);
}

std::string fsm_state_codec::to_msgpack(const fsm_state& v) {
    return utility::codec::object::write_msgpack(v, fsm_state_codec_fields);
}

fsm_state fsm_state_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<fsm_state>(data, fsm_state_codec_fields);
}

}
