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
#include "ores.qt/WorkunitCreateDialog.hpp"

#include <QVBoxLayout>
#include <QFormLayout>
#include <QDialogButtonBox>
#include <QPushButton>
#include <QApplication>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.compute.api/domain/workunit.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

WorkunitCreateDialog::WorkunitCreateDialog(ClientManager* clientManager,
                                           ChangeReasonCache* changeReasonCache,
                                           QWidget* parent)
    : QDialog(parent),
      client_manager_(clientManager),
      change_reason_cache_(changeReasonCache),
      batch_model_(new ClientBatchModel(clientManager, this)),
      app_version_model_(new ClientAppVersionModel(clientManager, this)) {

    setWindowTitle(tr("New Work Unit"));
    setMinimumWidth(500);

    auto* layout = new QVBoxLayout(this);
    auto* form   = new QFormLayout;

    batch_combo_ = new QComboBox(this);
    form->addRow(tr("Batch *"), batch_combo_);

    app_version_combo_ = new QComboBox(this);
    form->addRow(tr("App version *"), app_version_combo_);

    input_uri_edit_ = new QLineEdit(this);
    input_uri_edit_->setPlaceholderText(tr("api/v1/compute/inputs/…"));
    form->addRow(tr("Input URI *"), input_uri_edit_);

    config_uri_edit_ = new QLineEdit(this);
    config_uri_edit_->setPlaceholderText(tr("api/v1/compute/configs/…"));
    form->addRow(tr("Config URI"), config_uri_edit_);

    priority_spin_ = new QSpinBox(this);
    priority_spin_->setRange(0, 100);
    priority_spin_->setValue(50);
    form->addRow(tr("Priority"), priority_spin_);

    redundancy_spin_ = new QSpinBox(this);
    redundancy_spin_->setRange(1, 10);
    redundancy_spin_->setValue(1);
    form->addRow(tr("Target redundancy"), redundancy_spin_);

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

    auto* buttons   = new QDialogButtonBox(this);
    auto* create_btn = buttons->addButton(tr("Create"), QDialogButtonBox::AcceptRole);
    buttons->addButton(QDialogButtonBox::Cancel);

    connect(create_btn, &QPushButton::clicked,
            this, &WorkunitCreateDialog::on_create_clicked);
    connect(buttons, &QDialogButtonBox::rejected,
            this, &QDialog::reject);

    layout->addWidget(buttons);

    // Populate combos asynchronously; connect signals before calling refresh
    connect(batch_model_, &ClientBatchModel::dataLoaded,
            this, &WorkunitCreateDialog::populate_batch_combo);
    connect(app_version_model_, &ClientAppVersionModel::dataLoaded,
            this, &WorkunitCreateDialog::populate_app_version_combo);

    batch_model_->refresh();
    app_version_model_->refresh();
}

void WorkunitCreateDialog::populate_batch_combo() {
    batch_combo_->clear();
    for (int i = 0; i < batch_model_->rowCount(); ++i) {
        const auto* b = batch_model_->getBatch(i);
        if (!b) continue;
        const QString label = QString::fromStdString(b->external_ref)
            + " [" + QString::fromStdString(b->status) + "]";
        batch_combo_->addItem(label,
            QString::fromStdString(boost::uuids::to_string(b->id)));
    }
}

void WorkunitCreateDialog::populate_app_version_combo() {
    app_version_combo_->clear();
    for (int i = 0; i < app_version_model_->rowCount(); ++i) {
        const auto* v = app_version_model_->getVersion(i);
        if (!v) continue;
        const QString label =
            QString::fromStdString(boost::uuids::to_string(v->app_id)) + " v"
            + QString::fromStdString(v->wrapper_version);
        app_version_combo_->addItem(label,
            QString::fromStdString(boost::uuids::to_string(v->id)));
    }
}

void WorkunitCreateDialog::on_create_clicked() {
    if (batch_combo_->currentIndex() < 0) {
        MessageBoxHelper::warning(this, tr("Validation"),
                                  tr("A batch is required."));
        return;
    }
    if (app_version_combo_->currentIndex() < 0) {
        MessageBoxHelper::warning(this, tr("Validation"),
                                  tr("An app version is required."));
        return;
    }
    if (input_uri_edit_->text().trimmed().isEmpty()) {
        MessageBoxHelper::warning(this, tr("Validation"),
                                  tr("Input URI is required."));
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

    const auto batch_id =
        boost::lexical_cast<boost::uuids::uuid>(
            batch_combo_->currentData().toString().toStdString());
    const auto app_version_id =
        boost::lexical_cast<boost::uuids::uuid>(
            app_version_combo_->currentData().toString().toStdString());

    compute::domain::workunit wu;
    wu.id                 = boost::uuids::random_generator()();
    wu.batch_id           = batch_id;
    wu.app_version_id     = app_version_id;
    wu.input_uri          = input_uri_edit_->text().trimmed().toStdString();
    wu.config_uri         = config_uri_edit_->text().trimmed().toStdString();
    wu.priority           = priority_spin_->value();
    wu.target_redundancy  = redundancy_spin_->value();
    wu.modified_by        = username;
    wu.performed_by       = username;
    wu.change_reason_code = reason_code;
    wu.change_commentary  = commentary;

    compute::messaging::save_workunit_request req;
    req.workunit            = std::move(wu);
    req.change_reason_code  = reason_code;
    req.change_commentary   = commentary;

    QApplication::setOverrideCursor(Qt::WaitCursor);
    const auto resp = client_manager_->process_authenticated_request(std::move(req));
    QApplication::restoreOverrideCursor();

    if (!resp || !resp->success) {
        const QString msg = resp ? QString::fromStdString(resp->message)
                                 : tr("No response from server");
        BOOST_LOG_SEV(lg(), error) << "Workunit create failed: " << msg.toStdString();
        MessageBoxHelper::critical(this, tr("Create Failed"), msg);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Workunit created.";
    emit workunitCreated();
    accept();
}

} // namespace ores::qt
