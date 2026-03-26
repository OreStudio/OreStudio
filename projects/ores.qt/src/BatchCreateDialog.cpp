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
#include "ores.qt/BatchCreateDialog.hpp"

#include <QVBoxLayout>
#include <QFormLayout>
#include <QDialogButtonBox>
#include <QPushButton>
#include <QApplication>
#include <boost/uuid/random_generator.hpp>
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.compute.api/domain/batch.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

BatchCreateDialog::BatchCreateDialog(ClientManager* clientManager,
                                     ChangeReasonCache* changeReasonCache,
                                     QWidget* parent)
    : QDialog(parent),
      client_manager_(clientManager),
      change_reason_cache_(changeReasonCache) {

    setWindowTitle(tr("New Batch"));
    setMinimumWidth(420);

    auto* layout = new QVBoxLayout(this);
    auto* form   = new QFormLayout;

    external_ref_edit_ = new QLineEdit(this);
    external_ref_edit_->setPlaceholderText(tr("e.g. RISK_RUN_2026_Q1"));
    form->addRow(tr("External reference *"), external_ref_edit_);

    reason_combo_ = new QComboBox(this);
    const auto reasons = change_reason_cache_->getReasonsForNew("common");
    for (const auto& r : reasons)
        reason_combo_->addItem(QString::fromStdString(r.description),
                               QString::fromStdString(r.code));
    form->addRow(tr("Change reason *"), reason_combo_);

    commentary_edit_ = new QLineEdit(this);
    commentary_edit_->setPlaceholderText(tr("Optional"));
    form->addRow(tr("Commentary"), commentary_edit_);

    layout->addLayout(form);

    auto* buttons = new QDialogButtonBox(this);
    auto* create_btn = buttons->addButton(tr("Create"), QDialogButtonBox::AcceptRole);
    buttons->addButton(QDialogButtonBox::Cancel);

    connect(create_btn, &QPushButton::clicked,
            this, &BatchCreateDialog::on_create_clicked);
    connect(buttons, &QDialogButtonBox::rejected,
            this, &QDialog::reject);

    layout->addWidget(buttons);
}

void BatchCreateDialog::on_create_clicked() {
    const QString ext_ref = external_ref_edit_->text().trimmed();
    if (ext_ref.isEmpty()) {
        MessageBoxHelper::warning(this, tr("Validation"),
                                  tr("External reference is required."));
        return;
    }

    if (reason_combo_->currentIndex() < 0) {
        MessageBoxHelper::warning(this, tr("Validation"),
                                  tr("A change reason is required."));
        return;
    }

    const std::string reason_code = reason_combo_->currentData().toString().toStdString();
    const std::string commentary  = commentary_edit_->text().trimmed().toStdString();
    const std::string username    = client_manager_->storedUsername();

    compute::domain::batch batch;
    batch.id                 = boost::uuids::random_generator()();
    batch.external_ref       = ext_ref.toStdString();
    batch.status             = "open";
    batch.modified_by        = username;
    batch.performed_by       = username;
    batch.change_reason_code = reason_code;
    batch.change_commentary  = commentary;

    compute::messaging::save_batch_request req;
    req.batch               = std::move(batch);
    req.change_reason_code  = reason_code;
    req.change_commentary   = commentary;

    QApplication::setOverrideCursor(Qt::WaitCursor);
    const auto resp = client_manager_->process_authenticated_request(std::move(req));
    QApplication::restoreOverrideCursor();

    if (!resp || !resp->success) {
        const QString msg = resp ? QString::fromStdString(resp->message)
                                 : tr("No response from server");
        BOOST_LOG_SEV(lg(), error) << "Batch create failed: " << msg.toStdString();
        MessageBoxHelper::critical(this, tr("Create Failed"), msg);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Batch created: "
        << external_ref_edit_->text().toStdString();
    emit batchCreated();
    accept();
}

} // namespace ores::qt
