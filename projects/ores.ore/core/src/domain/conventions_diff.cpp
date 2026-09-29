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
#include <map>
#include <string>

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

} // namespace

std::string conventions_difference(const conventions& original,
                                   const conventions& exported,
                                   const std::string& path) {
    // Nothing the document carries may be missing from the export, and the
    // export may not invent an element the document did not have. This is what
    // catches a category the mapper reads and does not model: the mapper is
    // free to normalise a value, and not free to drop one.
    const auto from_document = element_counts(save_data(original));
    const auto from_export = element_counts(save_data(exported));
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
    // conventions out of each. The mapper collapses ORE's boolean spellings and
    // its enum aliases to the canonical codes the refdata columns hold, and
    // that normalisation is deliberate and carries the same information, so
    // comparing the mapped values tolerates it. A value the mapper got wrong
    // survives neither direction, so this catches it.
    if (conventions_mapper::map(original) != conventions_mapper::map(exported))
        return path + ": the export reads back as different conventions";

    return {};
}

}
