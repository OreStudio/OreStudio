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
#include "ores.qt/CountryDetailDialog.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QToolBar>
#include <QMdiSubWindow>
#include <QMetaObject>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ui_CountryDetailDialog.h"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/MdiUtils.hpp"
#include "ores.qt/FlagSelectorDialog.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.dq/domain/change_reason_constants.hpp"
#include "ores.refdata/messaging/protocol.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;
using FutureResult = std::pair<bool, std::string>;
namespace reason = dq::domain::change_reason_constants;

CountryDetailDialog::CountryDetailDialog(QWidget* parent)
    : DetailDialogBase(parent), ui_(new Ui::CountryDetailDialog), isDirty_(false),
      isAddMode_(false), isReadOnly_(false), isStale_(false), flagChanged_(false),
      historicalVersion_(0), flagButton_(nullptr),
      clientManager_(nullptr), imageCache_(nullptr), changeReasonCache_(nullptr),
      currentHistoryIndex_(0),
      firstVersionAction_(nullptr), prevVersionAction_(nullptr),
      nextVersionAction_(nullptr), lastVersionAction_(nullptr) {

    ui_->setupUi(this);

    // Create toolbar
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    // Define icon color (light gray for dark theme)
    const QColor iconColor(220, 220, 220);

    // Create Save action
    saveAction_ = new QAction("Save", this);
    saveAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Save, IconUtils::DefaultIconColor));
    saveAction_->setToolTip("Save changes");
    connect(saveAction_, &QAction::triggered, this,
        &CountryDetailDialog::onSaveClicked);
    toolBar_->addAction(saveAction_);

    // Create Delete action
    deleteAction_ = new QAction("Delete", this);
    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Delete, IconUtils::DefaultIconColor));
    deleteAction_->setToolTip("Delete country");
    connect(deleteAction_, &QAction::triggered, this,
        &CountryDetailDialog::onDeleteClicked);
    toolBar_->addAction(deleteAction_);

    toolBar_->addSeparator();

    // Create Revert action (initially hidden)
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert country to this historical version");
    connect(revertAction_, &QAction::triggered, this,
        &CountryDetailDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

    // Version navigation actions (initially hidden)
    toolBar_->addSeparator();

    firstVersionAction_ = new QAction("First", this);
    firstVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowPrevious, IconUtils::DefaultIconColor));
    firstVersionAction_->setToolTip(tr("First version"));
    connect(firstVersionAction_, &QAction::triggered, this,
        &CountryDetailDialog::onFirstVersionClicked);
    toolBar_->addAction(firstVersionAction_);
    firstVersionAction_->setVisible(false);

    prevVersionAction_ = new QAction("Previous", this);
    prevVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowLeft, IconUtils::DefaultIconColor));
    prevVersionAction_->setToolTip(tr("Previous version"));
    connect(prevVersionAction_, &QAction::triggered, this,
        &CountryDetailDialog::onPrevVersionClicked);
    toolBar_->addAction(prevVersionAction_);
    prevVersionAction_->setVisible(false);

    nextVersionAction_ = new QAction("Next", this);
    nextVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRight, IconUtils::DefaultIconColor));
    nextVersionAction_->setToolTip(tr("Next version"));
    connect(nextVersionAction_, &QAction::triggered, this,
        &CountryDetailDialog::onNextVersionClicked);
    toolBar_->addAction(nextVersionAction_);
    nextVersionAction_->setVisible(false);

    lastVersionAction_ = new QAction("Last", this);
    lastVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowNext, IconUtils::DefaultIconColor));
    lastVersionAction_->setToolTip(tr("Last version"));
    connect(lastVersionAction_, &QAction::triggered, this,
        &CountryDetailDialog::onLastVersionClicked);
    toolBar_->addAction(lastVersionAction_);
    lastVersionAction_->setVisible(false);

    // Add toolbar to the dialog's layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);

    // Add clickable flag button after toolbar
    if (mainLayout) {
        auto* flagContainer = new QWidget(this);
        auto* flagLayout = new QHBoxLayout(flagContainer);
        flagLayout->setContentsMargins(0, 4, 0, 4);

        flagButton_ = new QPushButton(this);
        flagButton_->setFixedSize(52, 52);
        flagButton_->setIconSize(QSize(48, 48));
        flagButton_->setFlat(true);
        flagButton_->setStyleSheet(
            "QPushButton { border: none; background: transparent; padding: 0px; } "
            "QPushButton:hover { background: rgba(255, 255, 255, 15); }");
        flagButton_->setCursor(Qt::PointingHandCursor);
        flagButton_->setToolTip(tr("Click to select flag"));
        connect(flagButton_, &QPushButton::clicked, this,
            &CountryDetailDialog::onSelectFlagClicked);
        flagLayout->addStretch();
        flagLayout->addWidget(flagButton_);
        flagLayout->addStretch();

        mainLayout->insertWidget(1, flagContainer);
    }

    // Connect signals for editable fields to detect changes
    connect(ui_->alpha2CodeEdit, &QLineEdit::textChanged, this,
        &CountryDetailDialog::onFieldChanged);
    connect(ui_->alpha3CodeEdit, &QLineEdit::textChanged, this,
        &CountryDetailDialog::onFieldChanged);
    connect(ui_->numericCodeEdit, &QLineEdit::textChanged, this,
        &CountryDetailDialog::onFieldChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
        &CountryDetailDialog::onFieldChanged);
    connect(ui_->officialNameEdit, &QLineEdit::textChanged, this,
        &CountryDetailDialog::onFieldChanged);

    // Initially disable save/reset buttons
    updateSaveResetButtonState();
}

void CountryDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CountryDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CountryDetailDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
    if (imageCache_) {
        connect(imageCache_, &ImageCache::countryImageSet,
            this, &CountryDetailDialog::onCountryImageSet);
        connect(imageCache_, &ImageCache::imagesLoaded,
            this, &CountryDetailDialog::updateFlagDisplay);
        connect(imageCache_, &ImageCache::allLoaded,
            this, &CountryDetailDialog::updateFlagDisplay);
    }
}

void CountryDetailDialog::setChangeReasonCache(ChangeReasonCache* changeReasonCache) {
    changeReasonCache_ = changeReasonCache;
}

CountryDetailDialog::~CountryDetailDialog() {
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CountryDetailDialog::setCountry(const refdata::domain::country& country) {
    currentCountry_ = country;
    isAddMode_ = country.alpha2_code.empty();

    if (country.image_id) {
        pendingImageId_ = QString::fromStdString(
            boost::uuids::to_string(*country.image_id));
    } else if (imageCache_) {
        std::string noFlagId = imageCache_->getNoFlagImageId();
        if (!noFlagId.empty()) {
            pendingImageId_ = QString::fromStdString(noFlagId);
        } else {
            pendingImageId_.clear();
        }
    } else {
        pendingImageId_.clear();
    }

    ui_->alpha2CodeEdit->setReadOnly(!isAddMode_);
    ui_->alpha2CodeEdit->setText(QString::fromStdString(country.alpha2_code));
    ui_->alpha3CodeEdit->setText(QString::fromStdString(country.alpha3_code));
    ui_->numericCodeEdit->setText(QString::fromStdString(country.numeric_code));
    ui_->nameEdit->setText(QString::fromStdString(country.name));
    ui_->officialNameEdit->setText(QString::fromStdString(country.official_name));
    ui_->versionEdit->setText(QString::number(country.version));
    ui_->modifiedByEdit->setText(QString::fromStdString(country.modified_by));
    ui_->performedByEdit->setText(QString::fromStdString(country.performed_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(country.recorded_at));
    ui_->changeReasonEdit->setText(QString::fromStdString(country.change_reason_code));
    ui_->commentaryEdit->setText(QString::fromStdString(country.change_commentary));

    isDirty_ = false;
    flagChanged_ = false;
    emit isDirtyChanged(false);
    updateSaveResetButtonState();
    updateFlagDisplay();
}

refdata::domain::country CountryDetailDialog::getCountry() const {
    refdata::domain::country country = currentCountry_;
    country.alpha2_code = ui_->alpha2CodeEdit->text().toUpper().toStdString();
    country.alpha3_code = ui_->alpha3CodeEdit->text().toUpper().toStdString();
    country.numeric_code = ui_->numericCodeEdit->text().toStdString();
    country.name = ui_->nameEdit->text().toStdString();
    country.official_name = ui_->officialNameEdit->text().toStdString();
    country.modified_by = username_.empty() ? "qt_user" : username_;

    if (!pendingImageId_.isEmpty()) {
        boost::uuids::string_generator gen;
        country.image_id = gen(pendingImageId_.toStdString());
    }

    return country;
}

void CountryDetailDialog::clearDialog() {
    ui_->alpha2CodeEdit->clear();
    ui_->alpha3CodeEdit->clear();
    ui_->numericCodeEdit->clear();
    ui_->nameEdit->clear();
    ui_->officialNameEdit->clear();
    ui_->versionEdit->clear();
    ui_->modifiedByEdit->clear();
    ui_->performedByEdit->clear();
    ui_->recordedAtEdit->clear();
    ui_->changeReasonEdit->clear();
    ui_->commentaryEdit->clear();
    pendingImageId_.clear();

    isDirty_ = false;
    flagChanged_ = false;
    emit isDirtyChanged(false);
    updateSaveResetButtonState();
}

void CountryDetailDialog::save() {
    onSaveClicked();
}

void CountryDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for country: "
                               << currentCountry_.alpha2_code;

    refdata::domain::country country = getCountry();

    // For updates (not creates), require change reason
    if (!isAddMode_) {
        if (!changeReasonCache_ || !changeReasonCache_->isLoaded()) {
            BOOST_LOG_SEV(lg(), warn) << "Change reasons not loaded, cannot save.";
            emit errorMessage("Change reasons not loaded. Please try again.");
            return;
        }

        // Get reasons for the "common" category that apply to amendments
        auto reasons = changeReasonCache_->getReasonsForAmend(
            std::string{reason::categories::common});
        if (reasons.empty()) {
            BOOST_LOG_SEV(lg(), warn) << "No change reasons available for common category.";
            emit errorMessage("No change reasons available. Please contact administrator.");
            return;
        }

        ChangeReasonDialog dialog(reasons, ChangeReasonDialog::OperationType::Amend,
            isDirty_, this);
        if (dialog.exec() != QDialog::Accepted) {
            BOOST_LOG_SEV(lg(), debug) << "Save cancelled - change reason dialog rejected.";
            return;
        }

        // Set the change reason on the country
        country.change_reason_code = dialog.selectedReasonCode();
        country.change_commentary = dialog.commentary();

        BOOST_LOG_SEV(lg(), debug) << "Change reason selected: "
                                   << country.change_reason_code
                                   << ", commentary: " << country.change_commentary;
    }

    QPointer<CountryDetailDialog> self = this;
    QFuture<FutureResult> future =
        QtConcurrent::run([self, country]() -> FutureResult {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending save country request for: "
                                       << country.alpha2_code;

            refdata::messaging::save_country_request request{country};
            auto payload = request.serialize();
            frame request_frame = frame(message_type::save_country_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result)
                return {false, "Failed to communicate with server"};

            BOOST_LOG_SEV(lg(), debug) << "Received save country response.";

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result)
                return {false, "Failed to decompress server response"};

            using refdata::messaging::save_country_response;
            auto response = save_country_response::deserialize(*payload_result);

            bool result = false;
            std::string message = "Invalid server response";
            if (response) {
                result = response->success;
                message = response->message;
            }

            return {result, message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, country]() {

        if (!self)
            return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Country saved successfully.";

            self->pendingImageId_.clear();

            self->isDirty_ = false;
            self->flagChanged_ = false;
            emit self->isDirtyChanged(false);
            self->updateSaveResetButtonState();

            QString code = QString::fromStdString(country.alpha2_code);
            if (self->isAddMode_) {
                emit self->countryCreated(code);
            } else {
                emit self->countryUpdated(code);
            }

            self->notifySaveSuccess(tr("Country '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Country save failed: " << message;
            emit self->errorMessage(QString("Failed to save country: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void CountryDetailDialog::onResetClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Reset clicked for country: "
                               << currentCountry_.alpha2_code;
    setCountry(currentCountry_);
}

void CountryDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete request for country: "
                               << currentCountry_.alpha2_code;

    auto reply =
        MessageBoxHelper::question(this, "Delete Country",
        QString("Are you sure you want to delete country '%1' (%2)?")
        .arg(QString::fromStdString(currentCountry_.name))
        .arg(QString::fromStdString(currentCountry_.alpha2_code)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<CountryDetailDialog> self = this;
    const std::string alpha2_code = currentCountry_.alpha2_code;
    QFuture<FutureResult> future =
        QtConcurrent::run([self, alpha2_code]() -> FutureResult {
            if (!self) return {false, {}};

            BOOST_LOG_SEV(lg(), debug) << "Sending delete country request for: "
                                       << alpha2_code;

            refdata::messaging::delete_country_request request{{alpha2_code}};
            auto payload = request.serialize();

            frame request_frame(message_type::delete_country_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to communicate with server.";
                return {false, "Failed to communicate with server."};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received delete country response.";

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress server response";
                return {false, "Failed to decompress server response"};
            }

            auto response = refdata::messaging::delete_country_response::deserialize(
                *payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Invalid server response";
                return {false, "Invalid server response"};
            }

            if (response->results.empty()) {
                BOOST_LOG_SEV(lg(), error) << "Empty results in response";
                return {false, "Empty results in response"};
            }

            return {response->results[0].success, response->results[0].message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, alpha2_code]() {

        BOOST_LOG_SEV(lg(), debug) << "Received country delete result.";
        if (!self) return;
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Country deleted successfully.";
            emit self->statusMessage(QString("Successfully deleted country: %1")
                .arg(QString::fromStdString(alpha2_code)));
            emit self->countryDeleted(QString::fromStdString(alpha2_code));
            self->clearDialog();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Country deletion failed: " << message;
            emit self->errorMessage(QString("Failed to delete country: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void CountryDetailDialog::onFieldChanged() {
    if (isReadOnly_)
        return;

    isDirty_ = true;
    emit isDirtyChanged(true);
    updateSaveResetButtonState();
}

void CountryDetailDialog::onRevertClicked() {
    BOOST_LOG_SEV(lg(), info) << "Revert clicked for historical version "
                              << historicalVersion_;

    auto reply = MessageBoxHelper::question(this, "Revert Country",
        QString("Are you sure you want to revert '%1' to version %2?\n\n"
                "This will create a new version with the data from version %2.")
            .arg(QString::fromStdString(currentCountry_.alpha2_code))
            .arg(historicalVersion_),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Revert cancelled by user";
        return;
    }

    emit revertRequested(currentCountry_);
}

void CountryDetailDialog::setReadOnly(bool readOnly, int versionNumber) {
    isReadOnly_ = readOnly;
    historicalVersion_ = versionNumber;

    BOOST_LOG_SEV(lg(), debug) << "Setting read-only mode: " << readOnly
                               << ", version: " << versionNumber;

    setFieldsReadOnly(readOnly);

    if (saveAction_)
        saveAction_->setVisible(!readOnly);

    if (deleteAction_)
        deleteAction_->setVisible(!readOnly);

    if (flagButton_)
        flagButton_->setEnabled(!readOnly);

    if (revertAction_)
        revertAction_->setVisible(readOnly);

    updateSaveResetButtonState();
}

void CountryDetailDialog::setFieldsReadOnly(bool readOnly) {
    ui_->alpha2CodeEdit->setReadOnly(readOnly);
    ui_->alpha3CodeEdit->setReadOnly(readOnly);
    ui_->numericCodeEdit->setReadOnly(readOnly);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->officialNameEdit->setReadOnly(readOnly);
}

void CountryDetailDialog::updateSaveResetButtonState() {
    if (isReadOnly_) {
        if (saveAction_)
            saveAction_->setEnabled(false);
        if (deleteAction_)
            deleteAction_->setEnabled(false);
        if (revertAction_)
            revertAction_->setEnabled(true);
        return;
    }

    // In add mode, only enable save when dirty
    // In edit mode, always enable save (for "touch" operations that update timestamp)
    if (saveAction_)
        saveAction_->setEnabled(isAddMode_ ? isDirty_ : true);

    if (deleteAction_)
        deleteAction_->setEnabled(!isAddMode_);
}

void CountryDetailDialog::markAsStale() {
    if (isStale_)
        return;

    isStale_ = true;
    BOOST_LOG_SEV(lg(), info) << "Country detail data marked as stale for: "
                              << currentCountry_.alpha2_code;

    MdiUtils::markParentWindowAsStale(this);

    emit statusMessage(QString("Country %1 has been modified on the server")
        .arg(QString::fromStdString(currentCountry_.alpha2_code)));
}

QString CountryDetailDialog::alpha2Code() const {
    return QString::fromStdString(currentCountry_.alpha2_code);
}

void CountryDetailDialog::onSelectFlagClicked() {
    if (!imageCache_) {
        BOOST_LOG_SEV(lg(), warn) << "Flag clicked but no image cache available.";
        emit errorMessage("Image cache not available");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Opening flag selector for: "
                               << currentCountry_.alpha2_code;

    // Get current image ID - use pending if set, otherwise from the country
    QString currentImageId = pendingImageId_.isEmpty()
        ? (currentCountry_.image_id
            ? QString::fromStdString(boost::uuids::to_string(*currentCountry_.image_id))
            : QString())
        : pendingImageId_;

    FlagSelectorDialog dialog(imageCache_, currentImageId, this);
    if (dialog.exec() == QDialog::Accepted) {
        QString selectedImageId = dialog.selectedImageId();
        BOOST_LOG_SEV(lg(), debug) << "Selected flag image: "
                                   << selectedImageId.toStdString();

        pendingImageId_ = selectedImageId;
        flagChanged_ = true;

        updateFlagDisplay();

        if (!isDirty_) {
            isDirty_ = true;
            emit isDirtyChanged(true);
            updateSaveResetButtonState();
        }

        emit statusMessage(tr("Flag changed. Click Save to apply."));
    }
}

void CountryDetailDialog::onCountryImageSet(const QString& alpha2_code,
    bool success, const QString& message) {

    if (alpha2_code.toStdString() != currentCountry_.alpha2_code) {
        return;
    }

    if (success) {
        BOOST_LOG_SEV(lg(), info) << "Flag updated successfully for: "
                                  << currentCountry_.alpha2_code;
        emit statusMessage(tr("Flag updated for %1")
            .arg(QString::fromStdString(currentCountry_.alpha2_code)));
        updateFlagDisplay();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to update flag for "
                                   << currentCountry_.alpha2_code << ": "
                                   << message.toStdString();
        emit errorMessage(tr("Failed to update flag: %1").arg(message));
        MessageBoxHelper::critical(this, "Flag Update Failed", message);
    }
    pendingImageId_.clear();
}

void CountryDetailDialog::updateFlagDisplay() {
    if (!flagButton_)
        return;

    if (flagChanged_) {
        flagButton_->setStyleSheet(
            "QPushButton { border: 2px solid orange; background: transparent; "
            "padding: 0px; border-radius: 4px; } "
            "QPushButton:hover { background: rgba(255, 255, 255, 15); }");
        flagButton_->setToolTip(tr("Flag changed (unsaved)"));
    } else {
        flagButton_->setStyleSheet(
            "QPushButton { border: none; background: transparent; padding: 0px; } "
            "QPushButton:hover { background: rgba(255, 255, 255, 15); }");
        flagButton_->setToolTip(tr("Click to select flag"));
    }

    if (!imageCache_) {
        return;
    }

    QString imageIdToShow;

    if (flagChanged_) {
        imageIdToShow = pendingImageId_;
    } else if (currentCountry_.image_id) {
        // Get image_id from the country domain object
        imageIdToShow = QString::fromStdString(
            boost::uuids::to_string(*currentCountry_.image_id));
    }

    if (!imageIdToShow.isEmpty()) {
        QIcon icon = imageCache_->getIcon(imageIdToShow.toStdString());
        if (!icon.isNull()) {
            flagButton_->setIcon(icon);
            return;
        }
    }

    QIcon noFlagIcon = imageCache_->getNoFlagIcon();
    if (!noFlagIcon.isNull()) {
        flagButton_->setIcon(noFlagIcon);
    }

    // Inline flag icon on alpha-2 code field
    const auto alpha2 = ui_->alpha2CodeEdit->text().trimmed().toStdString();
    QIcon inlineIcon = imageCache_->getCountryFlagIcon(alpha2);
    set_line_edit_flag_icon(ui_->alpha2CodeEdit, inlineIcon, alpha2FlagAction_);
}

void CountryDetailDialog::showVersionNavActions(bool visible) {
    if (firstVersionAction_)
        firstVersionAction_->setVisible(visible);
    if (prevVersionAction_)
        prevVersionAction_->setVisible(visible);
    if (nextVersionAction_)
        nextVersionAction_->setVisible(visible);
    if (lastVersionAction_)
        lastVersionAction_->setVisible(visible);
}

void CountryDetailDialog::setHistory(
    const std::vector<refdata::domain::country>& history, int versionNumber) {
    BOOST_LOG_SEV(lg(), debug) << "Setting history with " << history.size()
                               << " versions, displaying version " << versionNumber;

    history_ = history;

    // Find index of the requested version (history is newest-first)
    currentHistoryIndex_ = 0;
    for (size_t i = 0; i < history_.size(); ++i) {
        if (history_[i].version == versionNumber) {
            currentHistoryIndex_ = static_cast<int>(i);
            break;
        }
    }

    displayCurrentVersion();
    showVersionNavActions(true);
}

void CountryDetailDialog::displayCurrentVersion() {
    if (history_.empty() || currentHistoryIndex_ < 0 ||
        currentHistoryIndex_ >= static_cast<int>(history_.size())) {
        return;
    }

    const auto& country = history_[currentHistoryIndex_];

    // Display the country data
    setCountry(country);
    setReadOnly(true, country.version);

    // Update button states
    updateVersionNavButtonStates();

    // Gray out flag in history view - all versions are read-only
    if (flagButton_) {
        flagButton_->setEnabled(false);
        flagButton_->setToolTip(tr("Historical version - flag display only"));
    }

    // Update window title with short version format
    QWidget* parent = parentWidget();
    while (parent) {
        if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
            mdiSubWindow->setWindowTitle(QString("Country: %1 v%2")
                .arg(QString::fromStdString(country.alpha2_code))
                .arg(country.version));
            break;
        }
        parent = parent->parentWidget();
    }
}

void CountryDetailDialog::updateVersionNavButtonStates() {
    if (history_.empty()) {
        if (firstVersionAction_) firstVersionAction_->setEnabled(false);
        if (prevVersionAction_) prevVersionAction_->setEnabled(false);
        if (nextVersionAction_) nextVersionAction_->setEnabled(false);
        if (lastVersionAction_) lastVersionAction_->setEnabled(false);
        return;
    }

    bool atOldest = (currentHistoryIndex_ == static_cast<int>(history_.size()) - 1);
    bool atNewest = (currentHistoryIndex_ == 0);

    if (firstVersionAction_) firstVersionAction_->setEnabled(!atOldest);  // Go to oldest
    if (prevVersionAction_) prevVersionAction_->setEnabled(!atOldest);   // Go to older
    if (nextVersionAction_) nextVersionAction_->setEnabled(!atNewest);   // Go to newer
    if (lastVersionAction_) lastVersionAction_->setEnabled(!atNewest);   // Go to latest
}

void CountryDetailDialog::onFirstVersionClicked() {
    if (history_.empty()) return;

    BOOST_LOG_SEV(lg(), debug) << "Navigating to first (oldest) version";
    currentHistoryIndex_ = static_cast<int>(history_.size()) - 1;
    displayCurrentVersion();
}

void CountryDetailDialog::onPrevVersionClicked() {
    if (history_.empty()) return;

    if (currentHistoryIndex_ < static_cast<int>(history_.size()) - 1) {
        BOOST_LOG_SEV(lg(), debug) << "Navigating to previous (older) version";
        ++currentHistoryIndex_;
        displayCurrentVersion();
    }
}

void CountryDetailDialog::onNextVersionClicked() {
    if (history_.empty()) return;

    if (currentHistoryIndex_ > 0) {
        BOOST_LOG_SEV(lg(), debug) << "Navigating to next (newer) version";
        --currentHistoryIndex_;
        displayCurrentVersion();
    }
}

void CountryDetailDialog::onLastVersionClicked() {
    if (history_.empty()) return;

    BOOST_LOG_SEV(lg(), debug) << "Navigating to last (latest) version";
    currentHistoryIndex_ = 0;
    displayCurrentVersion();
}

}
