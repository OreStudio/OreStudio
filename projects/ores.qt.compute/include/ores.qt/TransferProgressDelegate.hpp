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
#ifndef ORES_QT_TRANSFER_PROGRESS_DELEGATE_HPP
#define ORES_QT_TRANSFER_PROGRESS_DELEGATE_HPP

#include <QStyledItemDelegate>

namespace ores::qt {

/**
 * @brief Renders a progress bar in the Progress column of ComputeTransferModel.
 *
 * Reads Qt::UserRole from the model index to obtain the integer progress value
 * (0–100) and paints a QStyleOptionProgressBar using the application style.
 */
class TransferProgressDelegate final : public QStyledItemDelegate {
    Q_OBJECT

public:
    explicit TransferProgressDelegate(QObject* parent = nullptr);

    void paint(QPainter* painter,
        const QStyleOptionViewItem& option,
        const QModelIndex& index) const override;

    QSize sizeHint(const QStyleOptionViewItem& option,
        const QModelIndex& index) const override;
};

} // namespace ores::qt

#endif
