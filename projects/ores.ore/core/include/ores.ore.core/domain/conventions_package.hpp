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
#ifndef ORES_ORE_CORE_DOMAIN_CONVENTIONS_PACKAGE_HPP
#define ORES_ORE_CORE_DOMAIN_CONVENTIONS_PACKAGE_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/generated/conventions.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.core/service/conventions_bundle.hpp"
#include <string>
#include <vector>

namespace ores::ore::domain {

/// The codec's record for one ORE conventions document.
using conventions = xml::generated::conventions::conventions;

/**
 * @brief One ORE conventions document, split at the family boundary.
 *
 * The bundle carries every family the document holds, one row per stored row,
 * and each row is that family's own payload. A document element that is more
 * than one row of a table, as a tenor basis swap is, arrives as one row per
 * table. This type names no family, so it does not change when the document
 * gains one; the two directions do, and each names every family it carries.
 *
 * The FX conventions are the exception, and are a projection rather than a
 * store. What the document calls one FX convention is a currency pair, that
 * pair's convention, and the calendars the pair advances on, and refdata
 * already holds all three. The projection therefore stays here, out of the
 * bundle, and never crosses to refdata.
 */
struct conventions_package {
    ores::refdata::service::conventions_bundle bundle;
    std::vector<ores::refdata::messaging::fx_convention> fx;
};

/**
 * @brief Reads one ORE document into the families it carries.
 *
 * The per-family field conversions are the mapper's, and the FX conventions
 * are kept aside rather than packed.
 */
ORES_ORE_CORE_EXPORT conventions_package split_conventions(const conventions& v);

/**
 * @brief Writes the families of one package back as an ORE document.
 *
 * The inverse of split_conventions, and the only path from a bundle back to a
 * document. A payload that does not decode into its family's type throws,
 * because a family that cannot be written must not be written as nothing.
 */
ORES_ORE_CORE_EXPORT conventions join_conventions(const conventions_package& p);

/**
 * @brief The names of the FX conventions the package carries and the store
 * does not keep.
 */
ORES_ORE_CORE_EXPORT std::vector<std::string> skipped_fx_conventions(const conventions_package& p);

}

#endif
