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
#ifndef ORES_MARKETDATA_API_DOMAIN_CATALOGUE_IDS_HPP
#define ORES_MARKETDATA_API_DOMAIN_CATALOGUE_IDS_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain::catalogue_ids {

/**
 * @brief The provider code of the Open Source Risk Engine.
 *
 * Series that the ORE import and the DQ publish create belong to this provider.
 */
inline constexpr std::string_view ore_provider = "ore";

/**
 * @brief The id of the source ore-import, of kind replay.
 *
 * The catalogue populate script installs the row under this id. The ORE import
 * and the DQ publish function record it on the observations they write.
 */
inline constexpr std::string_view ore_import_source = "26772cc0-57d6-4fac-be36-2ae75eb56b6b";

/**
 * @brief The provider codes of the providers that the simulator stands for, and the ids of their
 * simulated sources.
 *
 * A source name says which provider and which instance (bloomberg-a). It never says the kind, so
 * nothing downstream tells a simulated source from a live one. The catalogue populate script
 * installs the rows under these ids.
 */
inline constexpr std::string_view bloomberg_provider = "bloomberg";
inline constexpr std::string_view lseg_provider = "lseg";
inline constexpr std::string_view demo_provider = "demo";
inline constexpr std::string_view bloomberg_a_source = "0a42775e-3da7-4c61-a732-6b62cb9f948f";
inline constexpr std::string_view lseg_a_source = "c0f53bf1-a03a-4557-9906-7213091fcd82";
inline constexpr std::string_view demo_a_source = "8c0aa62c-3e8e-4174-9dca-010180c6fed0";

/**
 * @brief The id of the source ore-derived, of kind derived.
 *
 * The service computes the values of a bootstrapped curve, so the observations it
 * writes for one carry this source. The catalogue populate script installs the row.
 */
inline constexpr std::string_view ore_derived_source = "437b14be-6443-4492-b421-a51ce34f318a";

/**
 * @brief The id of the source composite-derived, of kind derived.
 *
 * The values of a composite are stored under this source. The catalogue populate script installs
 * the row.
 */
inline constexpr std::string_view composite_provider = "composite";
inline constexpr std::string_view composite_derived_source = "615e89cb-6a9c-46fb-beac-5b6d9b47a8ec";

inline boost::uuids::uuid composite_derived_source_id() {
    return boost::uuids::string_generator{}(std::string(composite_derived_source));
}

inline boost::uuids::uuid ore_import_source_id() {
    return boost::uuids::string_generator{}(std::string(ore_import_source));
}

inline boost::uuids::uuid ore_derived_source_id() {
    return boost::uuids::string_generator{}(std::string(ore_derived_source));
}

inline boost::uuids::uuid bloomberg_a_source_id() {
    return boost::uuids::string_generator{}(std::string(bloomberg_a_source));
}

inline boost::uuids::uuid lseg_a_source_id() {
    return boost::uuids::string_generator{}(std::string(lseg_a_source));
}

inline boost::uuids::uuid demo_a_source_id() {
    return boost::uuids::string_generator{}(std::string(demo_a_source));
}

/**
 * @brief The id of the simulated source of a provider, or nothing for a provider the simulator
 * does not stand for.
 */
inline std::optional<boost::uuids::uuid> simulated_source_id_for(std::string_view provider) {
    if (provider == bloomberg_provider)
        return bloomberg_a_source_id();
    if (provider == lseg_provider)
        return lseg_a_source_id();
    if (provider == demo_provider)
        return demo_a_source_id();
    return std::nullopt;
}

}

#endif
