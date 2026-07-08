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
#include "ores.qt.headless/HierarchyModelBuilder.hpp"
#include <QStandardItem>
#include <QStandardItemModel>
#include <QString>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

namespace {

QStandardItem* make_item(const ores::utility::domain::hierarchy_node& node) {
    auto* item = new QStandardItem(QString::fromStdString(node.name));
    item->setData(QString::fromStdString(boost::uuids::to_string(node.id)), Qt::UserRole);
    item->setEditable(false);

    for (const auto& child : node.children)
        item->appendRow(make_item(child));

    return item;
}

} // namespace

QStandardItemModel*
HierarchyModelBuilder::build(const std::vector<ores::utility::domain::hierarchy_node>& roots) {
    auto* model = new QStandardItemModel();
    model->setHorizontalHeaderLabels({QStringLiteral("Name")});

    for (const auto& root : roots)
        model->appendRow(make_item(root));

    return model;
}

}
