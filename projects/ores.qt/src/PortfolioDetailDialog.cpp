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
#include "ores.qt/PortfolioDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ui_PortfolioDetailDialog.h"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.dq/domain/change_reason_constants.hpp"
#include "ores.refdata/messaging/portfolio_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace reason = dq::domain::change_reason_constants;

PortfolioDetailDialog::PortfolioDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::PortfolioDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupConnections();
}

PortfolioDetailDialog::~PortfolioDetailDialog() {
    delete ui_;
}

QTabWidget* PortfolioDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* PortfolioDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* PortfolioDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void PortfolioDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PortfolioDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &PortfolioDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &PortfolioDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &PortfolioDetailDialog::onCloseClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &PortfolioDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->aggregationCcyCombo, &QComboBox::currentTextChanged, this,
            &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->purposeTypeCombo, &QComboBox::currentTextChanged, this,
            &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->portfolioTypeCombo, &QComboBox::currentTextChanged, this,
            &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->statusCombo, &QComboBox::currentTextChanged, this,
            &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->parentPortfolioCombo, &QComboBox::currentTextChanged, this,
            &PortfolioDetailDialog::onFieldChanged);
}

void PortfolioDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateCurrencyCombo();
    populatePurposeTypeCombo();
    populateParentPortfolioCombo();
}

void PortfolioDetailDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
    setup_flag_combo(this, ui_->aggregationCcyCombo, imageCache_, FlagSource::Currency);
}

void PortfolioDetailDialog::populateCurrencyCombo() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<PortfolioDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> std::vector<std::string> {
        return fetch_currency_codes(cm);
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished,
            self, [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        self->ui_->aggregationCcyCombo->clear();
        self->ui_->aggregationCcyCombo->addItem(QString());  // "(select)" sentinel
        for (const auto& code : codes) {
            self->ui_->aggregationCcyCombo->addItem(
                QString::fromStdString(code));
        }

        apply_flag_icons(self->ui_->aggregationCcyCombo, self->imageCache_,
                         FlagSource::Currency);

        self->updateUiFromPortfolio();
    });

    QFuture<std::vector<std::string>> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void PortfolioDetailDialog::populatePurposeTypeCombo() {
    ui_->purposeTypeCombo->clear();
    // Portfolio purpose classifications.
    // TODO: populate asynchronously from the purpose_types lookup service
    // once the protocol is available.
    for (const auto* pt : {"Risk", "Regulatory", "ClientReporting", "Internal"}) {
        ui_->purposeTypeCombo->addItem(QString::fromUtf8(pt));
    }
}

void PortfolioDetailDialog::populateParentPortfolioCombo() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<PortfolioDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> std::vector<portfolio_entry> {
        return fetch_portfolio_entries(cm);
    };

    auto* watcher = new QFutureWatcher<std::vector<portfolio_entry>>(self);
    connect(watcher, &QFutureWatcher<std::vector<portfolio_entry>>::finished,
            self, [self, watcher]() {
        auto entries = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        self->portfolioEntries_ = entries;
        self->ui_->parentPortfolioCombo->clear();
        self->ui_->parentPortfolioCombo->addItem(QString());  // "(none)" sentinel
        for (const auto& e : entries) {
            self->ui_->parentPortfolioCombo->addItem(
                QString::fromStdString(e.name));
        }
        self->updateUiFromPortfolio();
    });

    QFuture<std::vector<portfolio_entry>> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void PortfolioDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PortfolioDetailDialog::setChangeReasonCache(ChangeReasonCache* cache) {
    changeReasonCache_ = cache;
}

void PortfolioDetailDialog::setPortfolio(
    const refdata::domain::portfolio& portfolio) {
    portfolio_ = portfolio;
    updateUiFromPortfolio();
}

void PortfolioDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    if (createMode) {
        boost::uuids::random_generator gen;
        portfolio_.id = gen();
        if (clientManager_)
            portfolio_.party_id = clientManager_->currentPartyId();
    }
    ui_->deleteButton->setVisible(!createMode);

    setProvenanceEnabled(!createMode);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PortfolioDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->aggregationCcyCombo->setEnabled(!readOnly);
    ui_->purposeTypeCombo->setEnabled(!readOnly);
    ui_->portfolioTypeCombo->setEnabled(!readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
    ui_->parentPortfolioCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PortfolioDetailDialog::updateUiFromPortfolio() {
    ui_->nameEdit->setText(QString::fromStdString(portfolio_.name));
    ui_->partyIdLabel->setText(
        clientManager_ ? clientManager_->currentPartyName() : QString());
    ui_->descriptionEdit->setPlainText(QString::fromStdString(portfolio_.description));
    ui_->aggregationCcyCombo->setCurrentText(QString::fromStdString(portfolio_.aggregation_ccy));
    ui_->purposeTypeCombo->setCurrentText(QString::fromStdString(portfolio_.purpose_type));
    ui_->portfolioTypeCombo->setCurrentIndex(portfolio_.is_virtual != 0 ? 1 : 0);
    ui_->statusCombo->setCurrentText(QString::fromStdString(portfolio_.status));

    // Select the parent portfolio by matching the stored UUID to a loaded entry
    ui_->parentPortfolioCombo->setCurrentIndex(0);  // default: "(none)"
    if (portfolio_.parent_portfolio_id.has_value()) {
        const auto parent_id_str =
            boost::uuids::to_string(*portfolio_.parent_portfolio_id);
        for (const auto& e : portfolioEntries_) {
            if (e.id == parent_id_str) {
                ui_->parentPortfolioCombo->setCurrentText(
                    QString::fromStdString(e.name));
                break;
            }
        }
    }

    populateProvenance(portfolio_.version, portfolio_.modified_by,
        portfolio_.performed_by, portfolio_.recorded_at,
        portfolio_.change_reason_code, portfolio_.change_commentary);
    hasChanges_ = false;
    updateSaveButtonState();
}

void PortfolioDetailDialog::updatePortfolioFromUi() {
    portfolio_.name = ui_->nameEdit->text().trimmed().toStdString();
    portfolio_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    portfolio_.aggregation_ccy = ui_->aggregationCcyCombo->currentText().trimmed().toStdString();
    portfolio_.purpose_type = ui_->purposeTypeCombo->currentText().trimmed().toStdString();
    portfolio_.is_virtual = (ui_->portfolioTypeCombo->currentIndex() == 1) ? 1 : 0;
    portfolio_.status = ui_->statusCombo->currentText().trimmed().toStdString();
    portfolio_.modified_by = username_;
    portfolio_.performed_by = username_;

    // Resolve parent portfolio name back to UUID
    const auto parent_name =
        ui_->parentPortfolioCombo->currentText().trimmed().toStdString();
    portfolio_.parent_portfolio_id = std::nullopt;
    if (!parent_name.empty()) {
        for (const auto& e : portfolioEntries_) {
            if (e.name == parent_name) {
                portfolio_.parent_portfolio_id =
                    boost::lexical_cast<boost::uuids::uuid>(e.id);
                break;
            }
        }
    }
}

void PortfolioDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PortfolioDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PortfolioDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PortfolioDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();
    const QString ccy_val = ui_->aggregationCcyCombo->currentText().trimmed();

    return !name_val.isEmpty() && !ccy_val.isEmpty();
}

void PortfolioDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save portfolio while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields (name and currency are required).");
        return;
    }

    if (!createMode_) {
        if (!changeReasonCache_ || !changeReasonCache_->isLoaded()) {
            emit errorMessage("Change reasons not loaded. Please try again.");
            return;
        }
        auto reasons = changeReasonCache_->getReasonsForAmend(
            std::string{reason::categories::common});
        if (reasons.empty()) {
            emit errorMessage("No change reasons available. Please contact administrator.");
            return;
        }
        ChangeReasonDialog dlg(reasons, ChangeReasonDialog::OperationType::Amend,
                               hasChanges_, this);
        if (dlg.exec() != QDialog::Accepted)
            return;
        portfolio_.change_reason_code = dlg.selectedReasonCode();
        portfolio_.change_commentary  = dlg.commentary();
    }

    updatePortfolioFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving portfolio: " << portfolio_.name;

    QPointer<PortfolioDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, portfolio = portfolio_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_portfolio_request request;
        request.portfolios.push_back(portfolio);
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_portfolio_request,
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

        auto response = refdata::messaging::save_portfolio_response::
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
            BOOST_LOG_SEV(lg(), info) << "Portfolio saved successfully";
            QString code = QString::fromStdString(self->portfolio_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->portfolioSaved(code);
            self->notifySaveSuccess(tr("Portfolio '%1' saved").arg(code));
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

void PortfolioDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete portfolio while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(portfolio_.name);
    auto reply = MessageBoxHelper::question(this, "Delete Portfolio",
        QString("Are you sure you want to delete portfolio '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    if (changeReasonCache_ && changeReasonCache_->isLoaded()) {
        auto reasons = changeReasonCache_->getReasonsForAmend(
            std::string{reason::categories::common});
        if (!reasons.empty()) {
            ChangeReasonDialog dlg(reasons,
                ChangeReasonDialog::OperationType::Delete, true, this);
            if (dlg.exec() != QDialog::Accepted)
                return;
            BOOST_LOG_SEV(lg(), info) << "Delete reason: "
                << dlg.selectedReasonCode() << " — "
                << dlg.commentary();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting portfolio: " << portfolio_.name;

    QPointer<PortfolioDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = portfolio_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_portfolio_request request;
        request.ids.push_back({id});
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_portfolio_request,
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

        auto response = refdata::messaging::delete_portfolio_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Portfolio deleted successfully";
            emit self->statusMessage(QString("Portfolio '%1' deleted").arg(code));
            emit self->portfolioDeleted(code);
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
