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
#include <QStandardItemModel>
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <memory>

namespace {

const std::string tags("[hierarchy_model_builder]");

ores::qt::hierarchy_node
make_node(const boost::uuids::uuid& id, std::optional<boost::uuids::uuid> parent_id,
          const std::string& label) {
    ores::qt::hierarchy_node node;
    node.id = id;
    node.parent_id = parent_id;
    node.label = label;
    return node;
}

} // namespace

TEST_CASE("build_produces_single_root_with_nested_child_for_two_level_tree", tags) {
    auto rootId = boost::uuids::random_generator()();
    auto childId = boost::uuids::random_generator()();

    std::vector<ores::qt::hierarchy_node> nodes{
        make_node(rootId, std::nullopt, "Root"),
        make_node(childId, rootId, "Child"),
    };

    std::unique_ptr<QStandardItemModel> model(ores::qt::HierarchyModelBuilder::build(nodes));

    REQUIRE(model->rowCount() == 1);
    auto* rootItem = model->item(0);
    REQUIRE(rootItem != nullptr);
    CHECK(rootItem->text().toStdString() == "Root");
    REQUIRE(rootItem->rowCount() == 1);
    CHECK(rootItem->child(0)->text().toStdString() == "Child");
}

TEST_CASE("build_supports_multiple_roots", tags) {
    auto rootAId = boost::uuids::random_generator()();
    auto rootBId = boost::uuids::random_generator()();
    auto childOfAId = boost::uuids::random_generator()();

    std::vector<ores::qt::hierarchy_node> nodes{
        make_node(rootAId, std::nullopt, "Root A"),
        make_node(rootBId, std::nullopt, "Root B"),
        make_node(childOfAId, rootAId, "Child of A"),
    };

    std::unique_ptr<QStandardItemModel> model(ores::qt::HierarchyModelBuilder::build(nodes));

    REQUIRE(model->rowCount() == 2);

    bool foundRootA = false;
    bool foundRootB = false;
    for (int i = 0; i < model->rowCount(); ++i) {
        auto* item = model->item(i);
        if (item->text().toStdString() == "Root A") {
            foundRootA = true;
            REQUIRE(item->rowCount() == 1);
            CHECK(item->child(0)->text().toStdString() == "Child of A");
        } else if (item->text().toStdString() == "Root B") {
            foundRootB = true;
            CHECK(item->rowCount() == 0);
        }
    }
    CHECK(foundRootA);
    CHECK(foundRootB);
}

TEST_CASE("build_attaches_orphan_nodes_as_additional_roots", tags) {
    auto rootId = boost::uuids::random_generator()();
    auto orphanId = boost::uuids::random_generator()();
    auto missingParentId = boost::uuids::random_generator()();

    std::vector<ores::qt::hierarchy_node> nodes{
        make_node(rootId, std::nullopt, "Root"),
        // orphan's parent_id does not match any id in the input list.
        make_node(orphanId, missingParentId, "Orphan"),
    };

    std::unique_ptr<QStandardItemModel> model(ores::qt::HierarchyModelBuilder::build(nodes));

    // Both nodes should surface as roots: the model must not crash or
    // silently drop the orphan.
    REQUIRE(model->rowCount() == 2);

    bool foundRoot = false;
    bool foundOrphan = false;
    for (int i = 0; i < model->rowCount(); ++i) {
        auto* item = model->item(i);
        if (item->text().toStdString() == "Root")
            foundRoot = true;
        if (item->text().toStdString() == "Orphan")
            foundOrphan = true;
    }
    CHECK(foundRoot);
    CHECK(foundOrphan);
}

TEST_CASE("build_handles_empty_input", tags) {
    std::vector<ores::qt::hierarchy_node> nodes;
    std::unique_ptr<QStandardItemModel> model(ores::qt::HierarchyModelBuilder::build(nodes));

    CHECK(model->rowCount() == 0);
}
