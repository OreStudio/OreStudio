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
#include "ores.qt/CreateQueueDialog.hpp"

#include <QFormLayout>
#include <QVBoxLayout>
#include <QDialogButtonBox>
#include <QLabel>

namespace ores::qt {

CreateQueueDialog::CreateQueueDialog(QWidget* parent)
    : QDialog(parent) {

    setWindowTitle(tr("Create Queue"));
    setMinimumWidth(400);

    auto* layout = new QVBoxLayout(this);

    auto* formLayout = new QFormLayout();

    nameEdit_ = new QLineEdit(this);
    nameEdit_->setPlaceholderText(tr("e.g. my_queue"));
    nameEdit_->setMaxLength(64);
    formLayout->addRow(tr("Queue name:"), nameEdit_);

    scopeCombo_ = new QComboBox(this);
    scopeCombo_->addItem(tr("Party"),  QStringLiteral("party"));
    scopeCombo_->addItem(tr("Tenant"), QStringLiteral("tenant"));
    scopeCombo_->addItem(tr("System"), QStringLiteral("system"));
    formLayout->addRow(tr("Scope:"), scopeCombo_);

    typeCombo_ = new QComboBox(this);
    typeCombo_->addItem(tr("Task"),    QStringLiteral("task"));
    typeCombo_->addItem(tr("Channel"), QStringLiteral("channel"));
    formLayout->addRow(tr("Type:"), typeCombo_);

    descriptionEdit_ = new QLineEdit(this);
    descriptionEdit_->setPlaceholderText(tr("Optional description"));
    descriptionEdit_->setMaxLength(255);
    formLayout->addRow(tr("Description:"), descriptionEdit_);

    layout->addLayout(formLayout);

    auto* note = new QLabel(
        tr("<small>Queue names must start with a letter or underscore and contain "
           "only alphanumeric characters and underscores.</small>"), this);
    note->setWordWrap(true);
    layout->addWidget(note);

    auto* buttons = new QDialogButtonBox(this);
    createButton_ = buttons->addButton(tr("Create"), QDialogButtonBox::AcceptRole);
    buttons->addButton(QDialogButtonBox::Cancel);
    createButton_->setEnabled(false);

    connect(buttons, &QDialogButtonBox::accepted, this, &QDialog::accept);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    connect(nameEdit_, &QLineEdit::textChanged,
            this, &CreateQueueDialog::onNameChanged);

    layout->addWidget(buttons);
}

QString CreateQueueDialog::queueName() const {
    return nameEdit_->text().trimmed();
}

QString CreateQueueDialog::scopeType() const {
    return scopeCombo_->currentData().toString();
}

QString CreateQueueDialog::queueType() const {
    return typeCombo_->currentData().toString();
}

QString CreateQueueDialog::description() const {
    return descriptionEdit_->text().trimmed();
}

void CreateQueueDialog::onNameChanged(const QString& text) {
    createButton_->setEnabled(!text.trimmed().isEmpty());
}

}
