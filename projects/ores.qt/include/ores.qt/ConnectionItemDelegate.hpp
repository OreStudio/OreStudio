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
#ifndef ORES_QT_CONNECTION_ITEM_DELEGATE_HPP
#define ORES_QT_CONNECTION_ITEM_DELEGATE_HPP

#include <QStyledItemDelegate>
#include <QFont>

namespace ores::qt {

/**
 * @brief Custom delegate for rendering connection items with tag badges.
 *
 * Renders the Name column with tag badges displayed as colored pills
 * after the connection name.
 */
class ConnectionItemDelegate : public QStyledItemDelegate {
    Q_OBJECT

public:
    explicit ConnectionItemDelegate(QObject* parent = nullptr);

    void paint(QPainter* painter, const QStyleOptionViewItem& option,
               const QModelIndex& index) const override;

    QSize sizeHint(const QStyleOptionViewItem& option,
                   const QModelIndex& index) const override;

private:
    void drawTagBadge(QPainter* painter, const QRect& rect,
                      const QString& text, const QColor& backgroundColor,
                      const QFont& badgeFont) const;
};

}

#endif
