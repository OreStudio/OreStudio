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
#ifndef ORES_QT_CREATE_QUEUE_DIALOG_HPP
#define ORES_QT_CREATE_QUEUE_DIALOG_HPP

#include <QDialog>
#include <QComboBox>
#include <QLineEdit>
#include <QPushButton>

namespace ores::qt {

/**
 * @brief Dialog for creating a new message queue.
 *
 * Collects queue name, scope type (party/tenant/system), queue type
 * (task/channel), and an optional description.
 */
class CreateQueueDialog final : public QDialog {
    Q_OBJECT

public:
    explicit CreateQueueDialog(QWidget* parent = nullptr);

    QString queueName()   const;
    QString scopeType()   const;  // "party", "tenant", or "system"
    QString queueType()   const;  // "task" or "channel"
    QString description() const;

private slots:
    void onNameChanged(const QString& text);

private:
    QLineEdit*   nameEdit_;
    QComboBox*   scopeCombo_;
    QComboBox*   typeCombo_;
    QLineEdit*   descriptionEdit_;
    QPushButton* createButton_;
};

}

#endif
