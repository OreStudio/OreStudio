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
#include "ores.qt/TransferProgressDelegate.hpp"

#include <QApplication>
#include <QPainter>
#include <QStyleOptionProgressBar>

namespace ores::qt {

TransferProgressDelegate::TransferProgressDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {}

void TransferProgressDelegate::paint(
    QPainter* painter,
    const QStyleOptionViewItem& option,
    const QModelIndex& index) const {
    const int value = index.data(Qt::UserRole).toInt();

    QStyleOptionProgressBar bar;
    bar.rect     = option.rect;
    bar.minimum  = 0;
    bar.maximum  = 100;
    bar.progress = value;
    bar.text     = QString("%1%").arg(value);
    bar.textVisible = true;
    bar.state    = QStyle::State_Enabled | QStyle::State_Horizontal;

    QApplication::style()->drawControl(
        QStyle::CE_ProgressBar, &bar, painter);
}

QSize TransferProgressDelegate::sizeHint(
    const QStyleOptionViewItem& option,
    const QModelIndex& index) const {
    Q_UNUSED(index)
    // Match the row height; let the view decide the width.
    return {120, option.rect.height()};
}

} // namespace ores::qt
