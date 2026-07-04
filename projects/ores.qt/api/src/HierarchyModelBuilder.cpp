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
#include "ores.qt/HierarchyModelBuilder.hpp"
#include <QStandardItem>
#include <QStandardItemModel>
#include <QString>
#include <boost/functional/hash.hpp>
#include <boost/uuid/uuid_hash.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <unordered_map>
#include <unordered_set>

namespace ores::qt {

namespace {

QStandardItem* make_item(const hierarchy_node& node) {
    auto* item = new QStandardItem(QString::fromStdString(node.label));
    item->setData(QString::fromStdString(boost::uuids::to_string(node.id)), Qt::UserRole);
    item->setEditable(false);
    return item;
}

} // namespace

QStandardItemModel* HierarchyModelBuilder::build(const std::vector<hierarchy_node>& nodes) {
    auto* model = new QStandardItemModel();
    model->setHorizontalHeaderLabels({QStringLiteral("Name")});

    // Build a lookup from id -> item, taking the first occurrence of any
    // duplicated id (malformed input handled on a best-effort basis).
    std::unordered_map<boost::uuids::uuid, QStandardItem*, boost::hash<boost::uuids::uuid>>
        itemsById;
    std::unordered_set<boost::uuids::uuid, boost::hash<boost::uuids::uuid>> seenIds;

    for (const auto& node : nodes) {
        if (seenIds.contains(node.id))
            continue;
        seenIds.insert(node.id);
        itemsById.emplace(node.id, make_item(node));
    }

    // Attach each item to its parent's item if the parent exists in the
    // input, otherwise treat it as an additional root (orphan handling).
    for (const auto& node : nodes) {
        auto itemIt = itemsById.find(node.id);
        if (itemIt == itemsById.end())
            continue; // duplicate id, item already consumed by first occurrence

        QStandardItem* item = itemIt->second;
        QStandardItem* parentItem = nullptr;
        if (node.parent_id) {
            auto parentIt = itemsById.find(*node.parent_id);
            if (parentIt != itemsById.end())
                parentItem = parentIt->second;
        }

        if (parentItem != nullptr)
            parentItem->appendRow(item);
        else
            model->appendRow(item);
    }

    return model;
}

}
