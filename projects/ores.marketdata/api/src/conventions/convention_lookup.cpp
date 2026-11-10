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
#include "ores.marketdata.api/conventions/convention_lookup.hpp"
#include <format>
#include <optional>
#include <utility>

namespace ores::marketdata::conventions {

namespace {

using datum::market_index;
using datum::market_reference;

std::unexpected<lookup_error>
refuse(lookup_refusal_kind kind, const market_reference& asked, std::string reason) {
    return std::unexpected(lookup_error{kind, asked.uri(), std::move(reason)});
}

/// The refusals a lookup keeps aside, as the tail of a no-match reason.
template <typename Convention>
std::string refused_notes(const detail::answerable<Convention>& candidates, std::string_view what) {
    std::string notes;
    for (const auto& refused : candidates.refused_addresses)
        notes += std::format("; the {} '{}' states '{}', which the grammar refuses: {}",
                             what,
                             refused.id,
                             refused.address,
                             refused.reason);
    return notes;
}

template <typename Convention>
detail::answerable<Convention> answerable_of(std::vector<Convention> conventions) {
    detail::answerable<Convention> answerable;
    for (auto& convention : conventions) {
        if (!convention.oresmd_uri)
            continue;
        auto reference = market_reference::parse(*convention.oresmd_uri);
        if (!reference) {
            answerable.refused_addresses.push_back(typename detail::answerable<Convention>::refused{
                convention.id, *convention.oresmd_uri, std::move(reference.error())});
            continue;
        }
        answerable.readable.push_back(typename detail::answerable<Convention>::stated{
            std::move(*reference), std::move(convention)});
    }
    return answerable;
}

/*
 * The address of the index's family: the index with its tenor dropped. An
 * index that states no tenor is its own family.
 */
std::optional<market_index> family_address_of(const market_index& index) {
    std::vector<market_index::field_text> fields;
    for (const auto& field : index.fields())
        if (field.name != "tenor")
            fields.push_back(field);
    auto family = market_index::make(index.family(), index.subject(), std::move(fields));
    if (!family)
        return std::nullopt;
    return std::move(*family);
}

/*
 * The one convention of @p candidates that names @p wanted. Two that name it
 * are a conflict the caller must see, not a choice the lookup makes.
 */
template <typename Convention>
std::expected<Convention, lookup_error> the_one_at(const detail::answerable<Convention>& candidates,
                                                   const market_index& wanted,
                                                   const market_reference& asked,
                                                   std::string_view what) {
    const typename detail::answerable<Convention>::stated* found = nullptr;
    for (const auto& candidate : candidates.readable) {
        const auto* named = candidate.reference.index();
        if (named == nullptr || !(*named == wanted))
            continue;
        if (found != nullptr)
            return refuse(lookup_refusal_kind::more_than_one_match,
                          asked,
                          std::format("more than one {} names it: {} and {}",
                                      what,
                                      found->convention.id,
                                      candidate.convention.id));
        found = &candidate;
    }
    if (found == nullptr)
        return refuse(lookup_refusal_kind::no_match,
                      asked,
                      std::format("no {} names it{}", what, refused_notes(candidates, what)));
    return found->convention;
}

}

convention_lookup::convention_lookup(convention_set conventions)
    : ibor_indices_(answerable_of(std::move(conventions.ibor_indices)))
    , overnight_indices_(answerable_of(std::move(conventions.overnight_indices)))
    , swaps_(answerable_of(std::move(conventions.swaps)))
    , ois_swaps_(answerable_of(std::move(conventions.ois_swaps)))
    , cds_(std::move(conventions.cds)) {}

std::expected<index_definition, lookup_error>
convention_lookup::index_definition_of(const market_reference& index) const {
    const auto* named = index.index();
    if (named == nullptr)
        return refuse(lookup_refusal_kind::addresses_no_index,
                      index,
                      "the address names a datum, not an index");

    /*
     * Only the reducing indexes have a convention that defines them: an Ibor
     * index states its name and a tenor, and the convention states the name.
     * A CMS or a bond-yield index has no such convention, and its tenor cannot
     * be dropped, so it is refused as a question this lookup does not answer
     * rather than reported as a missing convention.
     */
    if (named->family() != datum::index_family::ibor)
        return refuse(lookup_refusal_kind::family_the_lookup_does_not_answer,
                      index,
                      std::format("a {} index is not defined by an Ibor or overnight index "
                                  "convention; this lookup answers the reducing indexes",
                                  datum::name_of(named->family())));

    const auto family = family_address_of(*named);
    if (!family)
        return refuse(lookup_refusal_kind::no_match,
                      index,
                      "its family address does not spell, so no convention can state it");

    auto ibor = the_one_at(ibor_indices_, *family, index, "Ibor index convention");
    auto overnight = the_one_at(overnight_indices_, *family, index, "overnight index convention");

    /*
     * Two conventions of one type that name the family are a fault the caller
     * must see, even when the other type answers: taking the answer that
     * happened to be unambiguous would hide it.
     */
    if (!ibor && ibor.error().kind == lookup_refusal_kind::more_than_one_match)
        return std::unexpected(ibor.error());
    if (!overnight && overnight.error().kind == lookup_refusal_kind::more_than_one_match)
        return std::unexpected(overnight.error());

    /*
     * ORE builds a few families both ways: USD-SOFR is the overnight index and
     * USD-SOFR-3M is a term rate, from different classes with different
     * conventions. The tenor the index states is what tells them apart, so a
     * family both types state is not a conflict; the kind decides.
     */
    const bool states_a_tenor = named->get("tenor") != nullptr;
    if (states_a_tenor && ibor)
        return index_definition{std::move(*ibor)};
    if (!states_a_tenor && overnight)
        return index_definition{std::move(*overnight)};
    if (ibor)
        return index_definition{std::move(*ibor)};
    if (overnight)
        return index_definition{std::move(*overnight)};
    return refuse(lookup_refusal_kind::no_match,
                  index,
                  std::format("no Ibor or overnight index convention defines it{}",
                              refused_notes(ibor_indices_, "Ibor index convention") +
                                  refused_notes(overnight_indices_, "overnight index convention")));
}

std::expected<refdata::domain::swap_convention, lookup_error>
convention_lookup::swap_convention_of(const market_reference& index) const {
    const auto* named = index.index();
    if (named == nullptr)
        return refuse(lookup_refusal_kind::addresses_no_index,
                      index,
                      "the address names a datum, not an index");
    return the_one_at(swaps_, *named, index, "swap convention");
}

std::expected<refdata::domain::ois_convention, lookup_error>
convention_lookup::ois_convention_of(const market_reference& index) const {
    const auto* named = index.index();
    if (named == nullptr)
        return refuse(lookup_refusal_kind::addresses_no_index,
                      index,
                      "the address names a datum, not an index");
    return the_one_at(ois_swaps_, *named, index, "OIS convention");
}

std::expected<refdata::domain::cds_convention, lookup_error>
convention_lookup::cds_convention_of(std::string_view id) const {
    const refdata::domain::cds_convention* found = nullptr;
    for (const auto& candidate : cds_) {
        if (candidate.id != id)
            continue;
        if (found != nullptr)
            return std::unexpected(lookup_error{lookup_refusal_kind::more_than_one_match,
                                                std::string(id),
                                                "more than one CDS convention has this id"});
        found = &candidate;
    }
    if (found == nullptr)
        return std::unexpected(lookup_error{
            lookup_refusal_kind::no_match, std::string(id), "no CDS convention has this id"});
    return *found;
}

}
