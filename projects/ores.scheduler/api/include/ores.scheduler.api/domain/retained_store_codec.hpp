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
 * Template: cpp_domain_type_codec.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SCHEDULER_API_DOMAIN_RETAINED_STORE_CODEC_HPP
#define ORES_SCHEDULER_API_DOMAIN_RETAINED_STORE_CODEC_HPP

#include "ores.scheduler.api/domain/retained_store.hpp"
#include "ores.scheduler.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::scheduler::domain {

/**
 * @brief The members of retained_store, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &retained_store::version},
        utility::codec::field{"tenant_id", &retained_store::tenant_id},
        utility::codec::field{"code", &retained_store::code},
        utility::codec::field{"display_name", &retained_store::display_name},
        utility::codec::field{"description", &retained_store::description},
        utility::codec::field{"owner_component", &retained_store::owner_component},
        utility::codec::field{"scope", &retained_store::scope},
        utility::codec::field{"target_kind", &retained_store::target_kind},
        utility::codec::field{"target", &retained_store::target},
        utility::codec::field{"time_column", &retained_store::time_column},
        utility::codec::field{"measure_kind", &retained_store::measure_kind},
        utility::codec::field{"bound_kind", &retained_store::bound_kind},
        utility::codec::field{"default_action", &retained_store::default_action},
        utility::codec::field{"default_retention_days", &retained_store::default_retention_days},
        utility::codec::field{"default_cadence_hours", &retained_store::default_cadence_hours},
        utility::codec::field{"default_reason", &retained_store::default_reason},
        utility::codec::field{"modified_by", &retained_store::modified_by},
        utility::codec::field{"performed_by", &retained_store::performed_by},
        utility::codec::field{"change_reason_code", &retained_store::change_reason_code},
        utility::codec::field{"change_commentary", &retained_store::change_commentary},
        utility::codec::field{"recorded_at", &retained_store::recorded_at},
    };
}

}

namespace ores::utility::codec {

/**
 * @brief The generated codec: what the wire codec resolves to for this type.
 *
 * The functions are declared here and defined in this type's own translation
 * unit, so a translation unit that only sends the type compiles a declaration
 * rather than a second copy of the encoder.
 */
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::domain::retained_store> {
    /**
     * @brief States that this is a generated codec.
     *
     * A test asserts this rather than the encoding, because the encoding is
     * what the type's own case already round-trips: what it
     * cannot see is whether the seam resolved to this specialisation or fell
     * through to the primary template, and a marker that only this
     * specialisation declares is that answer at compile time.
     */
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::domain::retained_store& v);

    static ores::scheduler::domain::retained_store read_json(std::string_view text);

    static std::string write_msgpack(const ores::scheduler::domain::retained_store& v);

    static ores::scheduler::domain::retained_store read_msgpack(std::span<const std::byte> data);
};

}

#endif
