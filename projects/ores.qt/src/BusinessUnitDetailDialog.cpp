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
#include "ores.qt/BusinessUnitDetailDialog.hpp"

#include <algorithm>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ui_BusinessUnitDetailDialog.h"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata/messaging/business_unit_protocol.hpp"
#include "ores.refdata/messaging/business_unit_type_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

BusinessUnitDetailDialog::BusinessUnitDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BusinessUnitDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupConnections();
}

BusinessUnitDetailDialog::~BusinessUnitDetailDialog() {
    delete ui_;
}

QTabWidget* BusinessUnitDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* BusinessUnitDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* BusinessUnitDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void BusinessUnitDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BusinessUnitDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &BusinessUnitDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &BusinessUnitDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &BusinessUnitDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &BusinessUnitDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &BusinessUnitDetailDialog::onFieldChanged);
    connect(ui_->businessCentreCombo, &QComboBox::currentTextChanged, this,
            &BusinessUnitDetailDialog::onFieldChanged);
    connect(ui_->statusCombo, &QComboBox::currentTextChanged, this,
            &BusinessUnitDetailDialog::onFieldChanged);
    connect(ui_->unitTypeCombo, &QComboBox::currentIndexChanged, this,
            [this](int index) {
        // Update the read-only level field whenever the type selection changes.
        // Index 0 is the "(none)" sentinel.
        if (index <= 0 || index > static_cast<int>(unit_type_entries_.size())) {
            ui_->unitTypeLevelEdit->clear();
        } else {
            const auto& entry = unit_type_entries_[static_cast<std::size_t>(index) - 1];
            ui_->unitTypeLevelEdit->setText(QString::number(entry.level));
        }
        onFieldChanged();
    });
}

void BusinessUnitDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateBusinessCentres();
    populateUnitTypes();
}

void BusinessUnitDetailDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
    setup_flag_combo(this, ui_->businessCentreCombo, imageCache_,
                     FlagSource::BusinessCentre);
}

void BusinessUnitDetailDialog::populateBusinessCentres() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<BusinessUnitDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> lookup_result {
        return fetch_party_lookups(cm);
    };

    auto* watcher = new QFutureWatcher<lookup_result>(self);
    connect(watcher, &QFutureWatcher<lookup_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        self->ui_->businessCentreCombo->clear();
        for (const auto& code : result.business_centre_codes) {
            self->ui_->businessCentreCombo->addItem(
                QString::fromStdString(code));
        }

        apply_flag_icons(self->ui_->businessCentreCombo, self->imageCache_,
                         FlagSource::BusinessCentre);

        self->updateUiFromUnit();
    });

    QFuture<lookup_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void BusinessUnitDetailDialog::populateUnitTypes() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<BusinessUnitDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> std::vector<unit_type_entry> {
        refdata::messaging::get_business_unit_types_request request;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_business_unit_types_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = refdata::messaging::get_business_unit_types_response::
            deserialize(*payload_result);
        if (!response) return {};

        std::vector<unit_type_entry> entries;
        entries.reserve(response->types.size());
        for (const auto& t : response->types) {
            entries.push_back({t.id, t.name, t.level});
        }
        std::sort(entries.begin(), entries.end(),
            [](const unit_type_entry& a, const unit_type_entry& b) {
                if (a.level != b.level) return a.level < b.level;
                return a.name < b.name;
            });
        return entries;
    };

    auto* watcher = new QFutureWatcher<std::vector<unit_type_entry>>(self);
    connect(watcher, &QFutureWatcher<std::vector<unit_type_entry>>::finished,
            self, [self, watcher]() {
        auto entries = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        self->unit_type_entries_ = entries;
        self->ui_->unitTypeCombo->clear();
        self->ui_->unitTypeCombo->addItem(QString("(none)"));
        for (const auto& e : entries) {
            self->ui_->unitTypeCombo->addItem(
                QString::fromStdString(e.name));
        }
        self->updateUiFromUnit();
    });

    QFuture<std::vector<unit_type_entry>> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void BusinessUnitDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BusinessUnitDetailDialog::setUnit(
    const refdata::domain::business_unit& business_unit) {
    business_unit_ = business_unit;
    updateUiFromUnit();
}

void BusinessUnitDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    setProvenanceEnabled(!createMode);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessUnitDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->businessCentreCombo->setEnabled(!readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
    ui_->unitTypeCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BusinessUnitDetailDialog::updateUiFromUnit() {
    ui_->codeEdit->setText(QString::fromStdString(business_unit_.unit_code));
    ui_->nameEdit->setText(QString::fromStdString(business_unit_.unit_name));
    ui_->businessCentreCombo->setCurrentText(QString::fromStdString(business_unit_.business_centre_code));
    ui_->statusCombo->setCurrentText(QString::fromStdString(business_unit_.status));

    // Select the unit type combo entry that matches the stored unit_type_id.
    ui_->unitTypeCombo->setCurrentIndex(0);
    ui_->unitTypeLevelEdit->clear();
    if (business_unit_.unit_type_id) {
        for (std::size_t i = 0; i < unit_type_entries_.size(); ++i) {
            if (unit_type_entries_[i].id == *business_unit_.unit_type_id) {
                const int combo_index = static_cast<int>(i) + 1; // +1 for "(none)"
                ui_->unitTypeCombo->setCurrentIndex(combo_index);
                ui_->unitTypeLevelEdit->setText(
                    QString::number(unit_type_entries_[i].level));
                break;
            }
        }
    }

    populateProvenance(business_unit_.version, business_unit_.modified_by,
                       business_unit_.performed_by, business_unit_.recorded_at,
                       business_unit_.change_reason_code,
                       business_unit_.change_commentary);
    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessUnitDetailDialog::updateUnitFromUi() {
    if (createMode_) {
        business_unit_.unit_code = ui_->codeEdit->text().trimmed().toStdString();
    }
    business_unit_.unit_name = ui_->nameEdit->text().trimmed().toStdString();
    business_unit_.business_centre_code = ui_->businessCentreCombo->currentText().trimmed().toStdString();
    business_unit_.status = ui_->statusCombo->currentText().trimmed().toStdString();
    business_unit_.modified_by = username_;
    business_unit_.performed_by = username_;

    // Resolve unit type combo selection back to an optional UUID.
    const int type_index = ui_->unitTypeCombo->currentIndex();
    if (type_index <= 0 || type_index > static_cast<int>(unit_type_entries_.size())) {
        business_unit_.unit_type_id = std::nullopt;
    } else {
        business_unit_.unit_type_id =
            unit_type_entries_[static_cast<std::size_t>(type_index) - 1].id;
    }
}

void BusinessUnitDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessUnitDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessUnitDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BusinessUnitDetailDialog::validateInput() {
    const QString unit_name_val = ui_->nameEdit->text().trimmed();

    return !unit_name_val.isEmpty();
}

void BusinessUnitDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save business unit while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateUnitFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving business unit: " << business_unit_.unit_code;

    QPointer<BusinessUnitDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, business_unit = business_unit_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_business_unit_request request;
        request.business_unit = business_unit;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_business_unit_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::save_business_unit_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Business Unit saved successfully";
            QString code = QString::fromStdString(self->business_unit_.unit_code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->business_unitSaved(code);
            self->notifySaveSuccess(tr("Business Unit '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void BusinessUnitDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete business unit while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(business_unit_.unit_code);
    auto reply = MessageBoxHelper::question(this, "Delete Business Unit",
        QString("Are you sure you want to delete business unit '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting business unit: " << business_unit_.unit_code;

    QPointer<BusinessUnitDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = business_unit_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_business_unit_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_business_unit_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::delete_business_unit_response::
            deserialize(*payload_result);

        if (!response || response->results.empty()) {
            return {false, "Invalid server response"};
        }

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Business Unit deleted successfully";
            emit self->statusMessage(QString("Business Unit '%1' deleted").arg(code));
            emit self->business_unitDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
