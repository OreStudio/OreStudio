/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/TagSelectorWidget.hpp"
#include "ores.qt/ConnectionTypes.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include <QLabel>
#include <QPushButton>
#include <QInputDialog>
#include <QMessageBox>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

TagSelectorWidget::TagSelectorWidget(connections::service::connection_manager* manager,
                                       QWidget* parent)
    : QWidget(parent),
      manager_(manager),
      layout_(new QHBoxLayout(this)),
      addButton_(new QToolButton(this)),
      tagMenu_(new QMenu(this)) {

    layout_->setContentsMargins(0, 0, 0, 0);
    layout_->setSpacing(4);

    // Add button setup
    addButton_->setText("+");
    addButton_->setToolTip(tr("Add tag"));
    addButton_->setFixedSize(24, 24);
    addButton_->setStyleSheet(
        "QToolButton {"
        "  background-color: #3B3B3B;"
        "  border: 1px solid #555555;"
        "  border-radius: 12px;"
        "  color: #CCCCCC;"
        "  font-weight: bold;"
        "}"
        "QToolButton:hover {"
        "  background-color: #4B4B4B;"
        "  border-color: #007ACC;"
        "}"
    );

    connect(addButton_, &QToolButton::clicked, this, &TagSelectorWidget::onAddTagClicked);
    connect(tagMenu_, &QMenu::triggered, this, &TagSelectorWidget::onTagActionTriggered);

    layout_->addWidget(addButton_);
    layout_->addStretch();

    setLayout(layout_);
}

void TagSelectorWidget::setSelectedTags(const std::vector<connections::domain::tag>& tags) {
    selectedTags_ = tags;
    rebuildTagDisplay();
}

std::vector<boost::uuids::uuid> TagSelectorWidget::selectedTagIds() const {
    std::vector<boost::uuids::uuid> ids;
    ids.reserve(selectedTags_.size());
    for (const auto& tag : selectedTags_) {
        ids.push_back(tag.id);
    }
    return ids;
}

void TagSelectorWidget::onAddTagClicked() {
    populateTagMenu();
    tagMenu_->popup(addButton_->mapToGlobal(QPoint(0, addButton_->height())));
}

void TagSelectorWidget::onTagActionTriggered(QAction* action) {
    QString actionData = action->data().toString();

    if (actionData == "create_new") {
        // Create new tag
        bool ok;
        QString name = QInputDialog::getText(this, tr("Create Tag"),
            tr("Tag name:"), QLineEdit::Normal, "", &ok);

        if (ok && !name.trimmed().isEmpty()) {
            name = name.trimmed();

            // Check if tag already exists
            auto existing = manager_->get_tag_by_name(name.toStdString());
            if (existing) {
                // Tag exists, add it if not already selected
                auto it = std::find_if(selectedTags_.begin(), selectedTags_.end(),
                    [&](const auto& t) { return t.id == existing->id; });
                if (it == selectedTags_.end()) {
                    selectedTags_.push_back(*existing);
                    rebuildTagDisplay();
                    emit selectionChanged();
                }
            } else {
                // Create new tag
                connections::domain::tag newTag;
                newTag.id = boost::uuids::random_generator()();
                newTag.name = name.toStdString();

                try {
                    manager_->create_tag(newTag);
                    selectedTags_.push_back(newTag);
                    rebuildTagDisplay();
                    emit selectionChanged();
                } catch (const std::exception& e) {
                    QMessageBox::warning(this, tr("Error"),
                        tr("Failed to create tag: %1").arg(e.what()));
                }
            }
        }
    } else {
        // Add existing tag
        boost::uuids::string_generator gen;
        boost::uuids::uuid tagId = gen(actionData.toStdString());

        auto tag = manager_->get_tag(tagId);
        if (tag) {
            // Check if not already selected
            auto it = std::find_if(selectedTags_.begin(), selectedTags_.end(),
                [&](const auto& t) { return t.id == tagId; });
            if (it == selectedTags_.end()) {
                selectedTags_.push_back(*tag);
                rebuildTagDisplay();
                emit selectionChanged();
            }
        }
    }
}

void TagSelectorWidget::removeTag(const boost::uuids::uuid& tagId) {
    auto it = std::find_if(selectedTags_.begin(), selectedTags_.end(),
        [&](const auto& t) { return t.id == tagId; });

    if (it != selectedTags_.end()) {
        selectedTags_.erase(it);
        rebuildTagDisplay();
        emit selectionChanged();
    }
}

void TagSelectorWidget::rebuildTagDisplay() {
    // Remove all widgets except the add button
    while (layout_->count() > 0) {
        QLayoutItem* item = layout_->takeAt(0);
        if (item->widget() && item->widget() != addButton_) {
            delete item->widget();
        }
        delete item;
    }

    // Add tag badges
    for (const auto& tag : selectedTags_) {
        QString name = QString::fromStdString(tag.name);
        QColor bgColor = colorForTag(name);

        // Create badge widget
        auto* badge = new QWidget(this);
        auto* badgeLayout = new QHBoxLayout(badge);
        badgeLayout->setContentsMargins(8, 2, 4, 2);
        badgeLayout->setSpacing(4);

        // Tag name label
        auto* label = new QLabel(name, badge);
        label->setStyleSheet(QString(
            "QLabel {"
            "  color: white;"
            "  font-size: 9pt;"
            "  font-weight: bold;"
            "}"
        ));
        badgeLayout->addWidget(label);

        // Remove button
        auto* removeBtn = new QPushButton("×", badge);
        removeBtn->setFixedSize(14, 14);
        removeBtn->setStyleSheet(
            "QPushButton {"
            "  background: transparent;"
            "  border: none;"
            "  color: rgba(255,255,255,0.7);"
            "  font-size: 12pt;"
            "  font-weight: bold;"
            "  padding: 0;"
            "}"
            "QPushButton:hover {"
            "  color: white;"
            "}"
        );

        boost::uuids::uuid tagId = tag.id;
        connect(removeBtn, &QPushButton::clicked, this, [this, tagId]() {
            removeTag(tagId);
        });
        badgeLayout->addWidget(removeBtn);

        badge->setStyleSheet(QString(
            "QWidget {"
            "  background-color: %1;"
            "  border-radius: 10px;"
            "}"
        ).arg(bgColor.name()));

        layout_->addWidget(badge);
    }

    // Add the add button and stretch at the end
    layout_->addWidget(addButton_);
    layout_->addStretch();
}

void TagSelectorWidget::populateTagMenu() {
    tagMenu_->clear();

    if (!manager_) {
        return;
    }

    try {
        auto allTags = manager_->get_all_tags();

        // Filter out already selected tags
        for (const auto& tag : allTags) {
            auto it = std::find_if(selectedTags_.begin(), selectedTags_.end(),
                [&](const auto& t) { return t.id == tag.id; });

            if (it == selectedTags_.end()) {
                QString name = QString::fromStdString(tag.name);
                QString idStr = QString::fromStdString(boost::uuids::to_string(tag.id));
                QAction* action = tagMenu_->addAction(name);
                action->setData(idStr);
            }
        }

        if (!tagMenu_->isEmpty()) {
            tagMenu_->addSeparator();
        }

        // Add "Create new..." option
        QAction* createAction = tagMenu_->addAction(tr("Create new tag..."));
        createAction->setData("create_new");

    } catch (const std::exception&) {
        QAction* createAction = tagMenu_->addAction(tr("Create new tag..."));
        createAction->setData("create_new");
    }
}

QColor TagSelectorWidget::colorForTag(const QString& name) const {
    // Generate consistent color based on tag name hash
    uint hash = qHash(name);
    return tag_colors[hash % tag_colors.size()];
}

}
