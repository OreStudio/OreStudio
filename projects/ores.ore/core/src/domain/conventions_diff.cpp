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
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <map>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::ore::domain {

namespace {

/// The distinct element names a document carries, and how often each appears.
std::map<std::string, int> element_counts(const std::string& xml) {
    std::map<std::string, int> names;
    std::size_t at = 0;
    while ((at = xml.find('<', at)) != std::string::npos) {
        if (xml.compare(at, 4, "<!--") == 0) {
            const auto end = xml.find("-->", at);
            at = end == std::string::npos ? xml.size() : end + 3;
            continue;
        }
        if (at + 1 < xml.size() && (xml[at + 1] == '?' || xml[at + 1] == '!')) {
            const auto end = xml.find('>', at);
            at = end == std::string::npos ? xml.size() : end + 1;
            continue;
        }
        std::size_t name_at = at + 1;
        if (name_at < xml.size() && xml[name_at] == '/')
            ++name_at;
        const auto end = xml.find_first_of(" \t\r\n/>", name_at);
        if (end == std::string::npos) {
            at = xml.size();
            continue;
        }
        if (end > name_at)
            ++names[xml.substr(name_at, end - name_at)];
        at = end;
    }
    return names;
}

constexpr std::string_view tenor_basis_swap_kind = "TENOR_BASIS_SWAP";

bool is_tenor_basis_swap_family(const std::string& family) {
    return family == "convention" || family == "tenor_basis_swap_pay_receive" ||
           family == "tenor_basis_swap_long_short";
}

/// The package without the families whose identity is not a value.
std::string without_tenor_basis_swap(const conventions_package& p) {
    std::string out;
    for (const auto& row : p.bundle.rows) {
        if (is_tenor_basis_swap_family(row.family))
            continue;
        out += row.family + "\x1f" + std::to_string(row.sequence) + "\x1f" + row.payload + "\n";
    }
    return out + rfl::json::write(p.fx);
}

template <typename Row>
Row decode(const refdata::service::conventions_bundle_row& row) {
    auto decoded = rfl::json::read<Row>(row.payload);
    if (!decoded)
        throw std::runtime_error("A conventions payload does not decode: " + row.family);
    return std::move(*decoded);
}

/// A tenor basis swap's facts, keyed by the identity the mapper states.
///
/// The mapper mints a provisional key for the central row and the part rows
/// reach it by that key, so two maps of one document agree on the key only
/// within a single map. What two maps do agree on is the stated identity, the
/// kind and the name, so the comparison pairs the conventions by that and drops
/// the provisional keys before it compares the rest.
std::map<std::string, std::string> tenor_basis_swap_facts(const conventions_package& p) {
    std::vector<refdata::domain::tenor_basis_swap_pay_receive> pay_receives;
    std::vector<refdata::domain::tenor_basis_swap_long_short> long_shorts;
    for (const auto& row : p.bundle.rows) {
        if (row.family == "tenor_basis_swap_pay_receive")
            pay_receives.push_back(decode<refdata::domain::tenor_basis_swap_pay_receive>(row));
        else if (row.family == "tenor_basis_swap_long_short")
            long_shorts.push_back(decode<refdata::domain::tenor_basis_swap_long_short>(row));
    }

    std::map<std::string, std::string> out;
    for (const auto& row : p.bundle.rows) {
        if (row.family != "convention")
            continue;
        const auto c = decode<refdata::domain::convention>(row);
        if (c.kind_code != tenor_basis_swap_kind)
            continue;
        refdata::messaging::tenor_basis_swap_convention_item item;
        auto central = c;
        central.id = boost::uuids::uuid{};
        item.convention = central;
        for (const auto& part : pay_receives) {
            if (part.convention_id != c.id)
                continue;
            auto copy = part;
            copy.id = boost::uuids::uuid{};
            copy.convention_id = boost::uuids::uuid{};
            item.pay_receive.push_back(copy);
        }
        for (const auto& part : long_shorts) {
            if (part.convention_id != c.id)
                continue;
            auto copy = part;
            copy.id = boost::uuids::uuid{};
            copy.convention_id = boost::uuids::uuid{};
            item.long_short.push_back(copy);
        }
        out[c.kind_code + "\x1f" + c.name] = rfl::json::write(item);
    }
    return out;
}

} // namespace

std::string conventions_difference(const conventions& original,
                                   const conventions& exported,
                                   const std::string& path) {
    // Nothing the document carries may be missing from the export, and the
    // export may not invent an element the document did not have. This is what
    // catches a category the mapper reads and does not model: the mapper is
    // free to normalise a value, and not free to drop one.
    const auto from_document = element_counts(conventions_mapper::write(original));
    const auto from_export = element_counts(conventions_mapper::write(exported));
    for (const auto& [name, count] : from_document) {
        const auto found = from_export.find(name);
        const int written = found == from_export.end() ? 0 : found->second;
        if (written != count)
            return path + ": <" + name + "> appears " + std::to_string(count) +
                   " time(s) in the document and " + std::to_string(written) + " in the export";
    }
    for (const auto& [name, count] : from_export) {
        if (!from_document.contains(name))
            return path + ": the export writes <" + name + ">, which the document did not have";
    }

    // The two documents are the same when the same mapper reads the same
    // conventions out of each, field for field. The mapper collapses ORE's boolean spellings and
    // its enum aliases to the canonical codes the refdata columns hold, and
    // that normalisation is deliberate and carries the same information, so
    // comparing the mapped values tolerates it. A value the mapper got wrong
    // survives neither direction, so this catches it.
    const auto mapped_document = split_conventions(original);
    const auto mapped_export = split_conventions(exported);
    if (without_tenor_basis_swap(mapped_document) != without_tenor_basis_swap(mapped_export))
        return path + ": the export reads back as different conventions";

    if (tenor_basis_swap_facts(mapped_document) != tenor_basis_swap_facts(mapped_export))
        return path + ": the export reads back as a different tenor basis swap";

    return {};
}

}
