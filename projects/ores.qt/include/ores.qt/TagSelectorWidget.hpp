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
#ifndef ORES_QT_TAG_SELECTOR_WIDGET_HPP
#define ORES_QT_TAG_SELECTOR_WIDGET_HPP

#include <QWidget>
#include <QHBoxLayout>
#include <QToolButton>
#include <QMenu>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.connections/domain/tag.hpp"

namespace ores::connections::service {
class connection_manager;
}

namespace ores::qt {

/**
 * @brief Widget for selecting and displaying tags as pill badges.
 *
 * Displays selected tags as colored pill badges with remove buttons.
 * Provides an add button to select from available tags or create new ones.
 */
class TagSelectorWidget : public QWidget {
    Q_OBJECT

public:
    explicit TagSelectorWidget(connections::service::connection_manager* manager,
                                QWidget* parent = nullptr);

    /**
     * @brief Set the currently selected tags.
     */
    void setSelectedTags(const std::vector<connections::domain::tag>& tags);

    /**
     * @brief Get the currently selected tag IDs.
     */
    std::vector<boost::uuids::uuid> selectedTagIds() const;

signals:
    /**
     * @brief Emitted when the selection changes.
     */
    void selectionChanged();

private slots:
    void onAddTagClicked();
    void onTagActionTriggered(QAction* action);
    void removeTag(const boost::uuids::uuid& tagId);

private:
    void rebuildTagDisplay();
    void populateTagMenu();
    QColor colorForTag(const QString& name) const;

    connections::service::connection_manager* manager_;
    QHBoxLayout* layout_;
    QToolButton* addButton_;
    QMenu* tagMenu_;

    std::vector<connections::domain::tag> selectedTags_;
};

}

#endif
