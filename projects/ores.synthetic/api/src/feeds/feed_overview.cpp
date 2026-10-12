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
#include "ores.synthetic.api/feeds/feed_overview.hpp"
#include <algorithm>
#include <cctype>
#include <unordered_map>

namespace ores::synthetic::feed {

namespace {

/// Names in the order a person reads them: letters without regard to case.
bool name_before(const std::string& a, const std::string& b) {
    return std::ranges::lexicographical_compare(a, b, [](unsigned char x, unsigned char y) {
        return std::tolower(x) < std::tolower(y);
    });
}

struct totals {
    int fx = 0;
    int ir = 0;
    int running = 0;
};

}

std::vector<messaging::feed_folder>
children_with_counts(const std::vector<messaging::feed_folder>& folders,
                     const std::vector<folder_feed_count>& direct,
                     const std::string& parent_id) {
    std::unordered_map<std::string, std::vector<const messaging::feed_folder*>> below;
    for (const auto& folder : folders)
        below[folder.parent_id].push_back(&folder);

    std::unordered_map<std::string, totals> own;
    for (const auto& count : direct) {
        auto& t = own[count.folder_id];
        t.fx += count.fx_feeds;
        t.ir += count.ir_feeds;
        t.running += count.running_feeds;
    }

    // Sums a folder and everything under it. A folder has one parent and the
    // database refuses a cycle, so the walk ends.
    const auto sum = [&](const auto& self, const std::string& id) -> totals {
        totals out;
        if (const auto found = own.find(id); found != own.end())
            out = found->second;
        if (const auto kids = below.find(id); kids != below.end()) {
            for (const auto* kid : kids->second) {
                const auto t = self(self, kid->id);
                out.fx += t.fx;
                out.ir += t.ir;
                out.running += t.running;
            }
        }
        return out;
    };

    std::vector<messaging::feed_folder> children;
    if (const auto found = below.find(parent_id); found != below.end()) {
        for (const auto* folder : found->second) {
            auto child = *folder;
            const auto t = sum(sum, child.id);
            child.fx_feeds = t.fx;
            child.ir_feeds = t.ir;
            child.running_feeds = t.running;
            child.has_children = below.contains(child.id);
            children.push_back(std::move(child));
        }
    }
    std::ranges::sort(children, [](const auto& a, const auto& b) {
        if (name_before(a.name, b.name))
            return true;
        if (name_before(b.name, a.name))
            return false;
        return a.id < b.id;
    });
    return children;
}

}
