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

#ifndef ORES_REFDATA_CORE_TESTS_CURVE_SEGMENT_SEED_HPP
#define ORES_REFDATA_CORE_TESTS_CURVE_SEGMENT_SEED_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/generators/convention_generator.hpp"
#include "ores.refdata.api/generators/curve_configuration_generator.hpp"
#include "ores.refdata.api/generators/curve_definition_generator.hpp"
#include "ores.refdata.api/generators/curve_section_generator.hpp"
#include "ores.refdata.api/generators/curve_segment_generator.hpp"
#include "ores.refdata.api/generators/curve_segment_type_generator.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_repository.hpp"
#include "ores.refdata.core/repository/curve_definition_repository.hpp"
#include "ores.refdata.core/repository/curve_section_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_type_repository.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::refdata::tests {

/**
 * Seeds a curve segment of one element, with the configuration, section,
 * definition and segment type it refers to, and returns its id.
 *
 * A segment part's insert trigger refuses a segment whose element does not have
 * that part, so a test of a part seeds its segment through this with an element
 * that has the part, rather than with whatever kind the generator names.
 */
inline boost::uuids::uuid seed_curve_segment_of_kind(
    utility::generation::generation_context& ctx,
    const database::context& party_ctx,
    std::string_view kind) {
    const auto party = *party_ctx.party_id();

    auto configuration = generators::generate_synthetic_curve_configuration(ctx);
    configuration.party_id = party;
    repository::curve_configuration_repository{}.write(party_ctx, configuration);

    auto section = generators::generate_synthetic_curve_section(ctx);
    repository::curve_section_repository{}.write(party_ctx, section);

    auto definition = generators::generate_synthetic_curve_definition(ctx);
    definition.party_id = party;
    definition.curve_configuration_id = configuration.id;
    definition.section_code = section.code;
    repository::curve_definition_repository{}.write(party_ctx, definition);

    auto type = generators::generate_synthetic_curve_segment_type(ctx);
    type.segment_kind = std::string(kind);
    repository::curve_segment_type_repository{}.write(party_ctx, type);

    auto segment = generators::generate_synthetic_curve_segment(ctx);
    segment.party_id = party;
    segment.curve_definition_id = definition.id;
    segment.segment_type = type.code;
    repository::curve_segment_repository{}.write(party_ctx, segment);
    return segment.id;
}

/**
 * Seeds a convention the curve segment convention part can name.
 */
inline void seed_convention_named(utility::generation::generation_context& ctx,
                                  const database::context& party_ctx,
                                  const std::string& name) {
    auto convention = generators::generate_synthetic_convention(ctx);
    convention.identity.party_id = *party_ctx.party_id();
    convention.payload.name = name;
    repository::convention_repository{}.write(party_ctx, convention);
}

}

#endif
